//////////////////////////////////////////////////////////////////////////////////////
//
//  The MIT License (MIT)
//
//  Copyright (c) 2015-present, Dom Chen.
//  All rights reserved.
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy of
//  this software and associated documentation files (the "Software"), to deal in the
//  Software without restriction, including without limitation the rights to use, copy,
//  modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
//  and to permit persons to whom the Software is furnished to do so, subject to the
//  following conditions:
//
//      The above copyright notice and this permission notice shall be included in all
//      copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
//  INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
//  PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
//  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
//  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
//  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
//////////////////////////////////////////////////////////////////////////////////////

namespace ts {


    interface Map<T> {
        [index: string]: T;
        [index: number]: T;
    }

    function createMap<T>(): Map<T> {
        const map: Map<T> = {};
        // Using 'delete' on an object causes V8 to put the object in dictionary mode.
        // This disables creation of hidden classes, which are expensive when an object is
        // constantly changing shape.
        map.__ = undefined as any;
        delete map.__;
        return map;
    }

    export function reorderSourceFiles(program: Program) {
        const sourceFiles = program.getSourceFiles();
        const rootFileNames = program.getRootFileNames();
        const checker = program.getTypeChecker();
        const visitedBlocks = [] as Block[];
        const dependencyMap = sourceFiles && createMap<string[]>();
        buildDependencyMap(sourceFiles);
        return sortOnDependency();

        function addDependency(file: string, dependent: string) {
            if (file === dependent) {
                return;
            }
            if (!dependencyMap) {
                return;
            }
            let list = dependencyMap[file];
            if (!list) {
                list = dependencyMap[file] = [];
            }
            if (list.indexOf(dependent) === -1) {
                list.push(dependent);
            }
        }

        function buildDependencyMap(sourceFiles: readonly SourceFile[]) {
            for (const sourceFile of sourceFiles) {
                if (sourceFile.isDeclarationFile) {
                    continue;
                }
                visitFile(sourceFile);
            }
        }

        function visitFile(sourceFile: SourceFile) {
            const statements = sourceFile.statements;
            const length = statements.length;
            for (let i = 0; i < length; i++) {
                const statement = statements[i];
                if (hasSyntacticModifier(statement, ModifierFlags.Ambient)) { // has the 'declare' keyword
                    continue;
                }
                visitStatement(statement);
            }
        }

        function visitStatement(statement?: Statement) {
            if (!statement) {
                return;
            }
            switch (statement.kind) {
                case SyntaxKind.ExpressionStatement:
                    const expression = <ExpressionStatement>statement;
                    visitExpression(expression.expression);
                    break;
                case SyntaxKind.ClassDeclaration:
                    checkInheriting(<ClassDeclaration>statement);
                    visitStaticMember(<ClassDeclaration>statement);
                    if (statement.transformFlags & TransformFlags.ContainsTypeScriptClassSyntax) {
                        visitClassDecorators(<ClassDeclaration>statement);
                    }
                    break;
                case SyntaxKind.VariableStatement:
                    visitVariableList((<VariableStatement>statement).declarationList);
                    break;
                case SyntaxKind.ImportEqualsDeclaration:
                    const importDeclaration = <ImportEqualsDeclaration>statement;
                    checkDependencyAtLocation(importDeclaration.moduleReference);
                    break;
                case SyntaxKind.ModuleDeclaration:
                    visitModule(<ModuleDeclaration>statement);
                    break;
                case SyntaxKind.Block:
                    visitBlock(<Block>statement);
                    break;
                case SyntaxKind.IfStatement:
                    const ifStatement = <IfStatement>statement;
                    visitExpression(ifStatement.expression);
                    visitStatement(ifStatement.thenStatement);
                    visitStatement(ifStatement.elseStatement);
                    break;
                case SyntaxKind.DoStatement:
                case SyntaxKind.WhileStatement:
                case SyntaxKind.WithStatement:
                    const doStatement = <DoStatement>statement;
                    visitExpression(doStatement.expression);
                    visitStatement(doStatement.statement);
                    break;
                case SyntaxKind.ForStatement:
                    {
                        const forStatement = <ForStatement>statement;
                        visitExpression(forStatement.condition);
                        visitExpression(forStatement.incrementor);
                        checkInitializer(forStatement.initializer);
                        break;
                    }
                case SyntaxKind.ForInStatement:
                case SyntaxKind.ForOfStatement:
                    {
                        const forInStatement = <ForInStatement>statement;
                        visitExpression(forInStatement.expression);
                        checkInitializer(forInStatement.initializer);
                        break;
                    }
                case SyntaxKind.ReturnStatement:
                    visitExpression((<ReturnStatement>statement).expression);
                    break;
                case SyntaxKind.SwitchStatement:
                    const switchStatment = <SwitchStatement>statement;
                    visitExpression(switchStatment.expression);
                    switchStatment.caseBlock.clauses.forEach(element => {
                        if (isCaseClause(element)) {
                            visitExpression(element.expression);
                        }
                        element.statements.forEach(element => {
                            visitStatement(element);
                        });
                    });
                    break;
                case SyntaxKind.LabeledStatement:
                    visitStatement((<LabeledStatement>statement).statement);
                    break;
                case SyntaxKind.ThrowStatement:
                    visitExpression((<ThrowStatement>statement).expression);
                    break;
                case SyntaxKind.TryStatement:
                    const tryStatement = <TryStatement>statement;
                    visitBlock(tryStatement.tryBlock);
                    visitBlock(tryStatement.finallyBlock);
                    if (tryStatement.catchClause) {
                        visitBlock(tryStatement.catchClause.block);
                    }
                    break;
            }
        }

        function checkInitializer(initializer?: ForInitializer) {
            if (initializer) {
                if (isVariableDeclarationList(initializer)) {
                    visitVariableList(initializer);
                }
                else {
                    visitExpression(initializer);
                }
            }
        }

        function visitModule(node: ModuleDeclaration) {
            const body = node.body;
            if (!body) {
                return;
            }
            if (isModuleDeclaration(body)) {
                visitModule(body);
                return;
            }
            if (isModuleBlock(body)) {
                for (const statement of body.statements) {
                    if (hasSyntacticModifier(statement, ModifierFlags.Ambient)) { // has the 'declare' keyword
                        continue;
                    }
                    visitStatement(statement);
                }
            }

        }

        function checkDependencyAtLocation(node: Node) {
            const symbol = checker && checker.getSymbolAtLocation(node);
            if (!symbol || !symbol.declarations) {
                return;
            }
            const sourceFile = getSourceFileOfNode(symbol.declarations[0]);
            if (!sourceFile || sourceFile.isDeclarationFile) {
                return;
            }
            addDependency(getSourceFileOfNode(node).fileName, sourceFile.fileName);
        }

        function checkInheriting(node: ClassDeclaration) {
            if (!node.heritageClauses) {
                return;
            }
            let heritageClause: HeritageClause | undefined;
            for (const clause of node.heritageClauses) {
                if (clause.token === SyntaxKind.ExtendsKeyword) {
                    heritageClause = clause;
                    break;
                }
            }
            if (!heritageClause) {
                return;
            }
            const superClasses = heritageClause.types;
            if (!superClasses) {
                return;
            }
            superClasses.forEach(superClass => {
                checkDependencyAtLocation(superClass.expression);
            });
        }

        function visitStaticMember(node: ClassDeclaration) {
            const members = node.members;
            if (!members) {
                return;
            }
            for (const member of members) {
                if (!hasSyntacticModifier(member, ModifierFlags.Static)) {
                    continue;
                }
                if (isPropertyDeclaration(member)) {
                    visitExpression(member.initializer);
                }
            }
        }

        function visitClassDecorators(node: ClassDeclaration) {
            if (node.decorators) {
                visitDecorators(node.decorators);
            }
            const members = node.members;
            if (!members) {
                return;
            }
            for (const member of members) {
                let decorators: NodeArray<Decorator> | undefined;
                let functionLikeMember: FunctionLikeDeclaration | undefined;
                if (isAccessor(member)) {
                    const accessors = getAllAccessorDeclarations(node.members, member);
                    const firstAccessor = accessors.firstAccessor;
                    if (member !== firstAccessor) {
                        continue;
                    }
                    decorators = firstAccessor.decorators;
                    if (!decorators) {
                        const secondAccessor = accessors.secondAccessor;
                        if (secondAccessor) {
                            decorators = secondAccessor.decorators;
                        }
                    }
                    functionLikeMember = accessors.setAccessor;
                }
                else {
                    decorators = member.decorators;
                    if (isMethodDeclaration(member)) {
                        functionLikeMember = member;
                    }
                }
                if (decorators) {
                    visitDecorators(decorators);
                }

                if (functionLikeMember) {
                    for (const parameter of functionLikeMember.parameters) {
                        if (parameter.decorators) {
                            visitDecorators(parameter.decorators);
                        }
                    }
                }
            }
        }

        function visitDecorators(decorators: NodeArray<Decorator>) {
            for (const decorator of decorators) {
                visitCallExpression(decorator.expression);
            }
        }

        function visitExpression(expression?: Expression) {
            if (!expression) {
                return;
            }
            switch (expression.kind) {
                case SyntaxKind.NewExpression:
                case SyntaxKind.CallExpression:
                    visitCallArguments(<CallExpression>expression);
                    visitCallExpression((<CallExpression>expression).expression);
                    break;
                case SyntaxKind.Identifier:
                    checkDependencyAtLocation(expression);
                    break;
                case SyntaxKind.PropertyAccessExpression:
                    checkDependencyAtLocation(expression);
                    break;
                case SyntaxKind.ElementAccessExpression:
                    visitExpression((<PropertyAccessExpression>expression).expression);
                    break;
                case SyntaxKind.ObjectLiteralExpression:
                    visitObjectLiteralExpression(<ObjectLiteralExpression>expression);
                    break;
                case SyntaxKind.ArrayLiteralExpression:
                    const arrayLiteral = <ArrayLiteralExpression>expression;
                    arrayLiteral.elements.forEach(visitExpression);
                    break;
                case SyntaxKind.TemplateExpression:
                    const template = <TemplateExpression>expression;
                    template.templateSpans.forEach(span => {
                        visitExpression(span.expression);
                    });
                    break;
                case SyntaxKind.ParenthesizedExpression:
                    const parenthesized = <ParenthesizedExpression>expression;
                    visitExpression(parenthesized.expression);
                    break;
                case SyntaxKind.BinaryExpression:
                    visitBinaryExpression(<BinaryExpression>expression);
                    break;
                case SyntaxKind.PostfixUnaryExpression:
                case SyntaxKind.PrefixUnaryExpression:
                    visitExpression((<PrefixUnaryExpression>expression).operand);
                    break;
                case SyntaxKind.DeleteExpression:
                    visitExpression((<DeleteExpression>expression).expression);
                    break;
                case SyntaxKind.TaggedTemplateExpression:
                    visitExpression((<TaggedTemplateExpression>expression).tag);
                    visitExpression((<TaggedTemplateExpression>expression).template);
                    break;
                case SyntaxKind.ConditionalExpression:
                    visitExpression((<ConditionalExpression>expression).condition);
                    visitExpression((<ConditionalExpression>expression).whenTrue);
                    visitExpression((<ConditionalExpression>expression).whenFalse);
                    break;

                case SyntaxKind.SpreadElement:
                    visitExpression((<SpreadElement>expression).expression);
                    break;
                case SyntaxKind.VoidExpression:
                    visitExpression((<VoidExpression>expression).expression);
                    break;
                case SyntaxKind.YieldExpression:
                    visitExpression((<YieldExpression>expression).expression);
                    break;
                case SyntaxKind.AwaitExpression:
                    visitExpression((<AwaitExpression>expression).expression);
                    break;
                case SyntaxKind.TypeOfExpression:
                    visitExpression((<TypeOfExpression>expression).expression);
                    break;
                case SyntaxKind.NonNullExpression:
                    visitExpression((<NonNullExpression>expression).expression);
                    break;
                case SyntaxKind.TypeAssertionExpression:
                    visitExpression((<TypeAssertion>expression).expression);
                    break;
            }

            // FunctionExpression
            // ArrowFunction
            // ClassExpression
            // OmittedExpression
            // ExpressionWithTypeArguments
            // AsExpression
        }

        function visitBinaryExpression(binary: BinaryExpression) {
            if (!checker) {
                return;
            }
            const { left, right } = binary;
            visitExpression(left);
            visitExpression(right);
            if (binary.operatorToken.kind === SyntaxKind.EqualsToken &&
                (left.kind === SyntaxKind.Identifier || left.kind === SyntaxKind.PropertyAccessExpression) &&
                (right.kind === SyntaxKind.Identifier || right.kind === SyntaxKind.PropertyAccessExpression)) {
                const symbol = checker.getSymbolAtLocation(left);
                if (!symbol || !symbol.declarations) {
                    return;
                }
                for (const declaration of symbol.declarations) {
                    if (declaration.kind === SyntaxKind.VariableDeclaration || declaration.kind === SyntaxKind.PropertyDeclaration) {
                        const variable = <VariableDeclaration>declaration;
                        if (variable.initializer) {
                            continue;
                        }
                        let delayInitializerList = variable.delayInitializerList;
                        if (!delayInitializerList) {
                            variable.delayInitializerList = delayInitializerList = [];
                        }
                        delayInitializerList.push(right);
                        if (variable.callerList) {
                            for (const callerFileName of variable.callerList) {
                                checkCallTarget(callerFileName, right);
                            }
                        }
                    }
                }
            }
        }

        function visitObjectLiteralExpression(objectLiteral: ObjectLiteralExpression) {
            objectLiteral.properties.forEach(element => {
                switch (element.kind) {
                    case SyntaxKind.PropertyAssignment:
                        visitExpression(element.initializer);
                        break;
                    case SyntaxKind.ShorthandPropertyAssignment:
                        visitExpression(element.objectAssignmentInitializer);
                        break;
                    case SyntaxKind.SpreadAssignment:
                        visitExpression(element.expression);
                        break;
                }
            });
        }

        function visitCallArguments(callExpression: CallExpression) {
            if (callExpression.arguments) {
                callExpression.arguments.forEach(argument => {
                    visitExpression(argument);
                });
            }
        }

        function visitCallExpression(expression: Expression) {
            expression = escapeParenthesized(expression);
            visitExpression(expression);
            switch (expression.kind) {
                case SyntaxKind.FunctionExpression:
                    const functionExpression = <FunctionExpression>expression;
                    visitBlock(functionExpression.body);
                    break;
                case SyntaxKind.PropertyAccessExpression:
                case SyntaxKind.Identifier:
                    const callerFileName = getSourceFileOfNode(expression).fileName;
                    checkCallTarget(callerFileName, expression);
                    break;
                case SyntaxKind.CallExpression:
                    visitReturnedFunction((<CallExpression>expression).expression);
                    break;
            }
        }

        function visitReturnedFunction(expression: Expression) {
            expression = escapeParenthesized(expression);
            let returnExpressions: Expression[] = [];
            if (isCallExpression(expression)) {
                const expressions = visitReturnedFunction(expression.expression);
                for (const returnExpression of expressions) {
                    const returns = visitReturnedFunction(returnExpression);
                    returnExpressions = returnExpressions.concat(returns);
                }
                return returnExpressions;
            }

            const functionBlocks: Block[] = [];
            switch (expression.kind) {
                case SyntaxKind.FunctionExpression:
                    functionBlocks.push((<FunctionExpression>expression).body);
                    break;
                case SyntaxKind.PropertyAccessExpression:
                case SyntaxKind.Identifier:
                    const callerFileName = getSourceFileOfNode(expression).fileName;
                    const declarations: Declaration[] = [];
                    getForwardDeclarations(expression, declarations, callerFileName);
                    for (const declaration of declarations) {
                        const sourceFile = getSourceFileOfNode(declaration);
                        if (!sourceFile || sourceFile.isDeclarationFile) {
                            continue;
                        }
                        if (isFunctionDeclaration(declaration) || isMethodDeclaration(declaration)) {
                            functionBlocks.push(declaration.body!);
                        }
                    }
                    break;
            }

            for (const block of functionBlocks) {
                for (const statement of block.statements) {
                    if (isReturnStatement(statement)) {
                        const returnExpression = statement.expression!;
                        returnExpressions.push(returnExpression);
                        visitCallExpression(returnExpression);
                    }
                }
            }
            return returnExpressions;
        }

        function escapeParenthesized(expression: Expression): Expression {
            if (expression.kind === SyntaxKind.ParenthesizedExpression) {
                return escapeParenthesized((<ParenthesizedExpression>expression).expression);
            }
            return expression;
        }

        function checkCallTarget(callerFileName: string, target: Node) {
            const declarations: Declaration[] = [];
            getForwardDeclarations(target, declarations, callerFileName);
            for (const declaration of declarations) {
                const sourceFile = getSourceFileOfNode(declaration);
                if (!sourceFile || sourceFile.isDeclarationFile) {
                    continue;
                }
                addDependency(callerFileName, sourceFile.fileName);
                if (isFunctionDeclaration(declaration) || isMethodDeclaration(declaration)) {
                    visitBlock(declaration.body);
                }
                else if (isClassDeclaration(declaration)) {
                    checkClassInstantiation(declaration);
                }
            }
        }

        function getForwardDeclarations(reference: Node, declarations: Declaration[], callerFileName: string) {
            const symbol = checker && checker.getSymbolAtLocation(reference);
            if (!symbol || !symbol.declarations) {
                return;
            }
            for (const declaration of symbol.declarations) {
                switch (declaration.kind) {
                    case SyntaxKind.FunctionDeclaration:
                    case SyntaxKind.MethodDeclaration:
                    case SyntaxKind.ClassDeclaration:
                        if (declarations.indexOf(declaration) === -1) {
                            declarations.push(declaration);
                        }
                        break;
                    case SyntaxKind.ImportEqualsDeclaration:
                        getForwardDeclarations((<ImportEqualsDeclaration>declaration).moduleReference, declarations, callerFileName);
                        break;
                    case SyntaxKind.VariableDeclaration:
                    case SyntaxKind.PropertyDeclaration:
                        const variable = <VariableDeclaration>declaration;
                        const initializer = variable.initializer;
                        if (initializer) {
                            if (initializer.kind === SyntaxKind.Identifier || initializer.kind === SyntaxKind.PropertyAccessExpression) {
                                getForwardDeclarations(initializer, declarations, callerFileName);
                            }
                        }
                        else {
                            if (variable.delayInitializerList) {
                                for (const expression of variable.delayInitializerList) {
                                    getForwardDeclarations(expression, declarations, callerFileName);
                                }
                            }
                            if (variable.callerList) {
                                if (variable.callerList.indexOf(callerFileName) === -1) {
                                    variable.callerList.push(callerFileName);
                                }
                            }
                            else {
                                variable.callerList = [callerFileName];
                            }
                        }
                        break;
                }
            }
        }

        function checkClassInstantiation(node: ClassDeclaration) {
            const members = node.members;
            if (!members) {
                return;
            }
            for (const member of members) {
                if (hasSyntacticModifier(member, ModifierFlags.Static)) {
                    continue;
                }
                if (isPropertyDeclaration(member)) {
                    visitExpression(member.initializer);
                }
                else if (isConstructorDeclaration(member)) {
                    visitBlock(member.body);
                }
            }
        }

        function visitBlock(block?: Block) {
            if (!block || visitedBlocks.indexOf(block) !== -1) {
                return;
            }
            visitedBlocks.push(block);
            for (const statement of block.statements) {
                visitStatement(statement);
            }
            visitedBlocks.pop();
        }

        function visitVariableList(variables?: VariableDeclarationList) {
            if (!variables) {
                return;
            }
            variables.declarations.forEach(declaration => {
                visitExpression(declaration.initializer);
            });
        }

        function sortOnDependency() {
            //使用副本进行操作
            const _sourceFiles = sourceFiles as SourceFile[];
            const _rootFileNames = rootFileNames as string[];
            const sortedFileNames = [] as string[];
            let circularReferences = [] as string[];
            const pathWeightMap = createMap<number>();
            const dtsFiles: SourceFile[] = [];
            const tsFiles: SourceFile[] = [];
            for (const sourceFile of _sourceFiles) {
                const path = sourceFile.fileName;
                if (sourceFile.isDeclarationFile) {
                    pathWeightMap[path] = 10000;
                    dtsFiles.push(sourceFile);
                    continue;
                }
                const references = updatePathWeight(path, 0, [path], pathWeightMap);
                if (references && references.length > 0) {
                    circularReferences = references;
                    break;
                }
                tsFiles.push(sourceFile);
            }
            if (circularReferences.length === 0) {
                tsFiles.sort(function (a: SourceFile, b: SourceFile): number {
                    return pathWeightMap[b.fileName] - pathWeightMap[a.fileName];
                });
                //将排序好的源码文件拷贝到原program中
                _sourceFiles.length = 0;
                _rootFileNames.length = 0;
                dtsFiles.concat(tsFiles).forEach(sourceFile => {
                    _sourceFiles.push(sourceFile);
                    _rootFileNames.push(sourceFile.fileName);
                    sortedFileNames.push(sourceFile.fileName);
                });
            }

            return { sortedFileNames, circularReferences };
        }

        function updatePathWeight(path: string, weight: number, references: string[], pathWeightMap: Map<number>): string[] | void {
            if (!pathWeightMap || !dependencyMap) {
                return;
            }
            if (pathWeightMap[path] === undefined) {
                pathWeightMap[path] = weight;
            }
            else {
                if (pathWeightMap[path] < weight) {
                    pathWeightMap[path] = weight;
                }
                else {
                    return;
                }
            }
            const list = dependencyMap[path];
            if (!list) {
                return;
            }
            for (const parentPath of list) {
                if (references.indexOf(parentPath) !== -1) {
                    references.push(parentPath);
                    return references;
                }
                const result = updatePathWeight(parentPath, weight + 1, references.concat(parentPath), pathWeightMap);
                if (result) {
                    return result;
                }
            }
            return;
        }

        function getSourceFileOfNode(node: Node): SourceFile {
            while (node && node.kind !== SyntaxKind.SourceFile) {
                node = node.parent;
            }
            return <SourceFile>node;
        }
    }
}