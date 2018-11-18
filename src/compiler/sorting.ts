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
        const map: Map<T> = Object.create(null);
        // Using 'delete' on an object causes V8 to put the object in dictionary mode.
        // This disables creation of hidden classes, which are expensive when an object is
        // constantly changing shape.
        map["__"] = <T><any>undefined;
        delete map["__"];
        return map;
    }

    export function reorderSourceFiles(program: Program) {
        const sourceFiles = program.getSourceFiles();
        const rootFileNames = program.getRootFileNames();
        const checker = program.getTypeChecker();
        const visitedBlocks = [] as Block[];
        const dependencyMap = buildDependencyMap(sourceFiles);
        return sortOnDependency();

        function addDependency(file: string, dependent: string) {
            if (file == dependent) {
                return;
            }
            if (!dependencyMap) {
                return;
            }
            let list = dependencyMap[file];
            if (!list) {
                list = dependencyMap[file] = [];
            }
            if (list.indexOf(dependent) == -1) {
                list.push(dependent);
            }
        }

        function buildDependencyMap(sourceFiles: ReadonlyArray<SourceFile>) {
            if (!sourceFiles) {
                return;
            }
            const dependencyMap = createMap<string[]>();
            for (let i = 0; i < sourceFiles.length; i++) {
                let sourceFile = sourceFiles[i];
                if (sourceFile.isDeclarationFile) {
                    continue;
                }
                visitFile(sourceFile);
            }
            return dependencyMap;
        }

        function visitFile(sourceFile: SourceFile) {
            let statements = sourceFile.statements;
            let length = statements.length;
            for (let i = 0; i < length; i++) {
                let statement = statements[i];
                if (hasModifier(statement, ModifierFlags.Ambient)) { // has the 'declare' keyword
                    continue;
                }
                visitStatement(statements[i]);
            }
        }

        function visitStatement(statement?: Statement) {
            if (!statement) {
                return;
            }
            switch (statement.kind) {
                case SyntaxKind.ExpressionStatement:
                    let expression = <ExpressionStatement>statement;
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
                    let importDeclaration = <ImportEqualsDeclaration>statement;
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
                    const forStatement = <ForStatement>statement;
                    visitExpression(forStatement.condition);
                    visitExpression(forStatement.incrementor);
                    if (forStatement.initializer) {
                        if (forStatement.initializer.kind === SyntaxKind.VariableDeclarationList) {
                            visitVariableList(<VariableDeclarationList>forStatement.initializer);
                        }
                        else {
                            visitExpression(<Expression>forStatement.initializer);
                        }
                    }
                    break;
                case SyntaxKind.ForInStatement:
                case SyntaxKind.ForOfStatement:
                    const forInStatement = <ForInStatement>statement;
                    visitExpression(forInStatement.expression);
                    if (forInStatement.initializer) {
                        if (forInStatement.initializer.kind === SyntaxKind.VariableDeclarationList) {
                            visitVariableList(<VariableDeclarationList>forInStatement.initializer);
                        }
                        else {
                            visitExpression(<Expression>forInStatement.initializer);
                        }
                    }
                    break;
                case SyntaxKind.ReturnStatement:
                    visitExpression((<ReturnStatement>statement).expression);
                    break;
                case SyntaxKind.SwitchStatement:
                    const switchStatment = <SwitchStatement>statement;
                    visitExpression(switchStatment.expression);
                    switchStatment.caseBlock.clauses.forEach(element => {
                        if (element.kind === SyntaxKind.CaseClause) {
                            visitExpression((<CaseClause>element).expression);
                        }
                        (<DefaultClause>element).statements.forEach(element => {
                            visitStatement(element);
                        })
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

        function visitModule(node: ModuleDeclaration) {
            let body = node.body;
            if (!body) {
                return;
            }
            if (body.kind === SyntaxKind.ModuleDeclaration) {
                visitModule(<ModuleDeclaration>node.body);
                return;
            }
            if (body.kind === SyntaxKind.ModuleBlock) {
                for (let statement of (<ModuleBlock>node.body).statements) {
                    if (hasModifier(statement, ModifierFlags.Ambient)) { // has the 'declare' keyword
                        continue;
                    }
                    visitStatement(statement);
                }
            }

        }

        function checkDependencyAtLocation(node: Node) {
            let symbol = checker && checker.getSymbolAtLocation(node);
            if (!symbol || !symbol.declarations) {
                return;
            }
            let sourceFile = getSourceFileOfNode(symbol.declarations[0]);
            if (!sourceFile || sourceFile.isDeclarationFile) {
                return;
            }
            addDependency(getSourceFileOfNode(node).fileName, sourceFile.fileName);
        }

        function checkInheriting(node: ClassDeclaration) {
            if (!node.heritageClauses) {
                return;
            }
            let heritageClause: HeritageClause | null = null;
            for (const clause of node.heritageClauses) {
                if (clause.token === SyntaxKind.ExtendsKeyword) {
                    heritageClause = clause;
                    break;
                }
            }
            if (!heritageClause) {
                return;
            }
            let superClasses = heritageClause.types;
            if (!superClasses) {
                return;
            }
            superClasses.forEach(superClass => {
                checkDependencyAtLocation(superClass.expression);
            });
        }

        function visitStaticMember(node: ClassDeclaration) {
            let members = node.members;
            if (!members) {
                return;
            }
            for (let member of members) {
                if (!hasModifier(member, ModifierFlags.Static)) {
                    continue;
                }
                if (member.kind == SyntaxKind.PropertyDeclaration) {
                    let property = <PropertyDeclaration>member;
                    visitExpression(property.initializer);
                }
            }
        }

        function visitClassDecorators(node: ClassDeclaration) {
            if (node.decorators) {
                visitDecorators(node.decorators);
            }
            let members = node.members;
            if (!members) {
                return;
            }
            for (let member of members) {
                let decorators: NodeArray<Decorator> | undefined;
                let functionLikeMember: FunctionLikeDeclaration | undefined;
                if (member.kind === SyntaxKind.GetAccessor || member.kind === SyntaxKind.SetAccessor) {
                    const accessors = getAllAccessorDeclarations(node.members, <AccessorDeclaration>member);
                    if (member !== accessors.firstAccessor) {
                        continue;
                    }
                    decorators = accessors.firstAccessor.decorators;
                    if (!decorators && accessors.secondAccessor) {
                        decorators = accessors.secondAccessor.decorators;
                    }
                    functionLikeMember = accessors.setAccessor;
                }
                else {
                    decorators = member.decorators;
                    if (member.kind === SyntaxKind.MethodDeclaration) {
                        functionLikeMember = <MethodDeclaration>member;
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
            for (let decorator of decorators) {
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
                    let arrayLiteral = <ArrayLiteralExpression>expression;
                    arrayLiteral.elements.forEach(visitExpression);
                    break;
                case SyntaxKind.TemplateExpression:
                    let template = <TemplateExpression>expression;
                    template.templateSpans.forEach(span => {
                        visitExpression(span.expression);
                    });
                    break;
                case SyntaxKind.ParenthesizedExpression:
                    let parenthesized = <ParenthesizedExpression>expression;
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
                case ts.SyntaxKind.TaggedTemplateExpression:
                    visitExpression((<TaggedTemplateExpression>expression).tag);
                    visitExpression((<TaggedTemplateExpression>expression).template);
                    break;
                case ts.SyntaxKind.ConditionalExpression:
                    visitExpression((<ts.ConditionalExpression>expression).condition);
                    visitExpression((<ts.ConditionalExpression>expression).whenTrue);
                    visitExpression((<ts.ConditionalExpression>expression).whenFalse);
                    break;

                case ts.SyntaxKind.SpreadElement:
                    visitExpression((<SpreadElement>expression).expression);
                    break;
                case ts.SyntaxKind.VoidExpression:
                    visitExpression((<VoidExpression>expression).expression);
                    break;
                case ts.SyntaxKind.YieldExpression:
                    visitExpression((<YieldExpression>expression).expression);
                    break;
                case ts.SyntaxKind.AwaitExpression:
                    visitExpression((<AwaitExpression>expression).expression);
                    break;
                case ts.SyntaxKind.TypeOfExpression:
                    visitExpression((<TypeOfExpression>expression).expression);
                    break;
                case ts.SyntaxKind.NonNullExpression:
                    visitExpression((<NonNullExpression>expression).expression);
                    break;
                case ts.SyntaxKind.TypeAssertionExpression:
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
            let left = binary.left;
            let right = binary.right;
            visitExpression(left);
            visitExpression(right);
            if (binary.operatorToken.kind === SyntaxKind.EqualsToken &&
                (left.kind === SyntaxKind.Identifier || left.kind === SyntaxKind.PropertyAccessExpression) &&
                (right.kind === SyntaxKind.Identifier || right.kind === SyntaxKind.PropertyAccessExpression)) {
                let symbol = checker.getSymbolAtLocation(left);
                if (!symbol || !symbol.declarations) {
                    return;
                }
                for (let declaration of symbol.declarations) {
                    if (declaration.kind === SyntaxKind.VariableDeclaration || declaration.kind === SyntaxKind.PropertyDeclaration) {
                        let variable = <VariableDeclaration>declaration;
                        if (variable.initializer) {
                            continue;
                        }
                        if (!variable.delayInitializerList) {
                            variable.delayInitializerList = [];
                        }
                        variable.delayInitializerList.push(right);
                        if (variable.callerList) {
                            for (let callerFileName of variable.callerList) {
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
                        visitExpression((<PropertyAssignment>element).initializer);
                        break;
                    case SyntaxKind.ShorthandPropertyAssignment:
                        visitExpression((<ShorthandPropertyAssignment>element).objectAssignmentInitializer);
                        break;
                    case SyntaxKind.SpreadAssignment:
                        visitExpression((<SpreadAssignment>element).expression);
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
                    let functionExpression = <FunctionExpression>expression;
                    visitBlock(functionExpression.body);
                    break;
                case SyntaxKind.PropertyAccessExpression:
                case SyntaxKind.Identifier:
                    let callerFileName = getSourceFileOfNode(expression).fileName;
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
            if (expression.kind === SyntaxKind.CallExpression) {
                let expressions = visitReturnedFunction((<CallExpression>expression).expression);
                for (let returnExpression of expressions) {
                    let returns = visitReturnedFunction(returnExpression);
                    returnExpressions = returnExpressions.concat(returns);
                }
                return returnExpressions;
            }

            let functionBlocks: Block[] = [];
            switch (expression.kind) {
                case SyntaxKind.FunctionExpression:
                    functionBlocks.push((<FunctionExpression>expression).body);
                    break;
                case SyntaxKind.PropertyAccessExpression:
                case SyntaxKind.Identifier:
                    let callerFileName = getSourceFileOfNode(expression).fileName;
                    let declarations: Declaration[] = [];
                    getForwardDeclarations(expression, declarations, callerFileName);
                    for (let declaration of declarations) {
                        let sourceFile = getSourceFileOfNode(declaration);
                        if (!sourceFile || sourceFile.isDeclarationFile) {
                            continue;
                        }
                        if (declaration.kind === SyntaxKind.FunctionDeclaration ||
                            declaration.kind === SyntaxKind.MethodDeclaration) {
                            functionBlocks.push((<FunctionDeclaration>declaration).body!);
                        }
                    }
                    break;
            }

            for (let block of functionBlocks) {
                for (let statement of block.statements) {
                    if (statement.kind === SyntaxKind.ReturnStatement) {
                        let returnExpression = (<ReturnStatement>statement).expression;
                        returnExpressions.push(returnExpression!);
                        visitCallExpression(returnExpression!);
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
            let declarations: Declaration[] = [];
            getForwardDeclarations(target, declarations, callerFileName);
            for (let declaration of declarations) {
                let sourceFile = getSourceFileOfNode(declaration);
                if (!sourceFile || sourceFile.isDeclarationFile) {
                    continue;
                }
                addDependency(callerFileName, sourceFile.fileName);
                if (declaration.kind === SyntaxKind.FunctionDeclaration ||
                    declaration.kind === SyntaxKind.MethodDeclaration) {
                    visitBlock((<FunctionDeclaration>declaration).body);
                }
                else if (declaration.kind === SyntaxKind.ClassDeclaration) {
                    checkClassInstantiation(<ClassDeclaration>declaration);
                }
            }
        }

        function getForwardDeclarations(reference: Node, declarations: Declaration[], callerFileName: string) {
            let symbol = checker && checker.getSymbolAtLocation(reference);
            if (!symbol || !symbol.declarations) {
                return;
            }
            for (let declaration of symbol.declarations) {
                switch (declaration.kind) {
                    case SyntaxKind.FunctionDeclaration:
                    case SyntaxKind.MethodDeclaration:
                    case SyntaxKind.ClassDeclaration:
                        if (declarations.indexOf(declaration) == -1) {
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
                                for (let expression of variable.delayInitializerList) {
                                    getForwardDeclarations(expression, declarations, callerFileName);
                                }
                            }
                            if (variable.callerList) {
                                if (variable.callerList.indexOf(callerFileName) == -1) {
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
            let members = node.members;
            if (!members) {
                return;
            }
            for (let member of members) {
                if (hasModifier(member, ModifierFlags.Static)) {
                    continue;
                }
                if (member.kind === SyntaxKind.PropertyDeclaration) {
                    let property = <PropertyDeclaration>member;
                    visitExpression(property.initializer);
                }
                else if (member.kind === SyntaxKind.Constructor) {
                    let constructor = <ConstructorDeclaration>member;
                    visitBlock(constructor.body);
                }
            }
        }

        function visitBlock(block?: Block) {
            if (!block || visitedBlocks.indexOf(block) != -1) {
                return;
            }
            visitedBlocks.push(block);
            for (let statement of block.statements) {
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
            let _sourceFiles = sourceFiles.concat();
            let _rootFileNames = rootFileNames.concat();
            let sortedFileNames = [] as string[];
            let circularReferences = [] as string[];
            let pathWeightMap = createMap<number>();
            let dtsFiles: SourceFile[] = [];
            let tsFiles: SourceFile[] = [];
            for (let sourceFile of _sourceFiles) {
                let path = sourceFile.fileName;
                if (sourceFile.isDeclarationFile) {
                    pathWeightMap[path] = 10000;
                    dtsFiles.push(sourceFile);
                    continue;
                }
                let references = updatePathWeight(path, 0, [path], pathWeightMap);
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
            let list = dependencyMap[path];
            if (!list) {
                return;
            }
            for (let parentPath of list) {
                if (references.indexOf(parentPath) != -1) {
                    references.push(parentPath);
                    return references;
                }
                let result = updatePathWeight(parentPath, weight + 1, references.concat(parentPath), pathWeightMap);
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