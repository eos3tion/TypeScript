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

/*@internal*/
namespace ts {

    export interface MethodDeclaration {
        isJumpTarget?: boolean;
    }
    export interface ClassLikeDeclarationBase {
        typeNames?: string[];
    }

    export interface Block {
        /*@internal*/ visitedBySorting?: boolean;
    }
    export interface VariableDeclaration {
        /* @internal */
        callerList?: string[];
        /* @internal */
        delayInitializerList?: Expression[];
    }

    export interface CompilerOptions {
        /* ts-plus options */
        accessorOptimization?: boolean;
        defines?: MapLike<any>;
        emitReflection?: boolean;
        noEmitJs?: boolean;
        reorderFiles?: boolean;
    }

    export interface EmitHost {
        /* @internal */
        getTypeChecker?(): TypeChecker;
    }

    export function transformTypeScriptPlus(context: TransformationContext) {
        const compilerOptions = context.getCompilerOptions();
        const compilerDefines = getCompilerDefines(compilerOptions.defines);
        const typeChecker = context.getEmitHost().getTypeChecker!();
        const previousOnSubstituteNode = context.onSubstituteNode;
        if (compilerDefines) {
            context.onSubstituteNode = onSubstituteNode;
            context.enableSubstitution(SyntaxKind.Identifier);
        }

        return chainBundle(context, transformSourceFile);

        function transformSourceFile(node: SourceFile) {
            if (!compilerOptions.emitReflection) {
                return node;
            }
            const visited = factory.updateSourceFile(node, visitNodes(node.statements, visitStatement, isStatement));
            addEmitHelpers(visited, context.readEmitHelpers());
            return visited;
        }

        function visitStatement(node: Node): VisitResult<Node> {
            if (hasSyntacticModifier(node, ModifierFlags.Ambient)) {
                return node;
            }
            if (node.kind === SyntaxKind.ClassDeclaration) {
                return visitClassDeclaration(<ClassDeclaration>node);
            }
            if (node.kind === SyntaxKind.ModuleDeclaration) {
                return visitModule(<NamespaceDeclaration>node);
            }
            return node;
        }

        function visitModule(node: NamespaceDeclaration): NamespaceDeclaration {
            const body = node.body;
            if (isModuleDeclaration(body)) {
                return updateModuleDeclaration(node, visitModule(body));
            }
            if (isModuleBlock(body)) {
                return updateModuleDeclaration(node, factory.updateModuleBlock(body, body.statements));
            }
            return node;
        }

        function updateModuleDeclaration(node: NamespaceDeclaration, body: ModuleBody) {
            if (node.body !== body) {
                return factory.updateModuleDeclaration(node, node.decorators, node.modifiers, node.name, body) as NamespaceDeclaration;
            }
            return node;
        }

        function visitClassDeclaration(node: ClassDeclaration): VisitResult<Statement> {
            const classStatement = factory.cloneNode(node);
            const statements: Statement[] = [classStatement];

            const interfaceMap: any = {};
            getImplementedInterfaces(node, interfaceMap);
            const allInterfaces = Object.keys(interfaceMap);
            let interfaces: string[];
            const superTypes = getSuperClassTypes(node);
            if (superTypes) {
                interfaces = [];
                for (const type of allInterfaces) {
                    if (superTypes.indexOf(type) === -1) {
                        interfaces.push(type);
                    }
                }
            }
            else {
                interfaces = allInterfaces;
            }
            node.typeNames = interfaces;
            const fullClassName = typeChecker.getFullyQualifiedName(node.symbol);
            const name = node.name!;
            const expression = createReflectHelper(context, name, fullClassName, interfaces);
            setSourceMapRange(expression, createRange(name.pos, node.end));

            const statement = factory.createExpressionStatement(expression);
            setSourceMapRange(statement, createRange(-1, node.end));
            statements.push(statement);

            return statements;
        }

        function getImplementedInterfaces(node: Node, result: any) {
            let superInterfaces: readonly ExpressionWithTypeArguments[] | undefined;
            if (node.kind === SyntaxKind.ClassDeclaration) {
                superInterfaces = getEffectiveImplementsTypeNodes(<ClassLikeDeclaration>node);
            }
            else {
                superInterfaces = getInterfaceBaseTypeNodes(<InterfaceDeclaration>node);
            }
            if (superInterfaces) {
                superInterfaces.forEach(superInterface => {
                    const type = typeChecker.getTypeAtLocation(superInterface);
                    if (type && type.symbol && type.symbol.flags & SymbolFlags.Interface) {
                        const symbol = type.symbol;
                        const fullName = typeChecker.getFullyQualifiedName(symbol);
                        result[fullName] = true;
                        const declaration = getDeclarationOfKind(symbol, SyntaxKind.InterfaceDeclaration);
                        if (declaration) {
                            getImplementedInterfaces(declaration, result);
                        }
                    }
                });
            }
        }

        function getSuperClassTypes(node: ClassLikeDeclaration) {
            const superClass = tryGetClassExtendingExpressionWithTypeArguments(node)!;
            if (!superClass) {
                return;
            }
            const type = typeChecker && typeChecker.getTypeAtLocation(superClass);
            if (!type || !type.symbol) {
                return;
            }
            const declaration = <ClassLikeDeclaration>getDeclarationOfKind(type.symbol, SyntaxKind.ClassDeclaration);
            return declaration && declaration.typeNames;
        }


        function getCompilerDefines(defines?: MapLike<any>) {
            const compilerDefines: MapLike<any> = {};
            if (defines) {
                const keys = Object.keys(defines);
                for (const key of keys) {
                    const value = defines[key];
                    const type = typeof value;
                    switch (type) {
                        case "boolean":
                        case "number":
                            compilerDefines[key] = value.toString();
                            break;
                        case "string":
                            compilerDefines[key] = "\"" + value + "\"";
                            break;
                    }
                }
            }
            return compilerDefines;
        }

        function isDefinedConstant(node: Identifier): boolean {
            if (compilerDefines[node.escapedText as string] === undefined) {
                return false;
            }
            if (!node.parent) {
                return false;
            }
            if (node.parent.kind === SyntaxKind.VariableDeclaration && (<VariableDeclaration>node.parent).name === node) {
                return false;
            }
            if (node.parent.kind === SyntaxKind.BinaryExpression) {
                const parent = <BinaryExpression>node.parent;
                if (parent.left === node && parent.operatorToken.kind === SyntaxKind.EqualsToken) {
                    return false;
                }
            }

            const symbol = typeChecker.getSymbolAtLocation(node);
            if (!symbol || !symbol.declarations) {
                return false;
            }
            const declaration = symbol.declarations[0];
            if (!declaration) {
                return false;
            }
            if (declaration.kind !== SyntaxKind.VariableDeclaration) {
                return false;
            }
            const statement = declaration.parent.parent;
            return (statement.parent.kind === SyntaxKind.SourceFile);
        }

        /**
         * Hooks node substitutions.
         * @param emitContext The context for the emitter.
         * @param node The node to substitute.
         */
        function onSubstituteNode(hint: EmitHint, node: Node) {
            node = previousOnSubstituteNode(hint, node);
            if (isIdentifier(node) && isDefinedConstant(node)) {
                return factory.createIdentifier(compilerDefines[node.escapedText as string]);
            }
            return node;
        }
    }

    const reflectHelper: EmitHelper = {
        name: "typescript:reflect",
        scoped: false,
        priority: 0,
        text: `
            var __reflect = (this && this.__reflect) || function (p, c, t) {
                p.__class__ = c, t ? t.push(c) : t = [c], p.__types__ = p.__types__ ? t.concat(p.__types__) : t;
            };`
    };

    function createReflectHelper(context: TransformationContext, name: Identifier, fullClassName: string, interfaces: string[]) {
        context.requestEmitHelper(reflectHelper);
        const argumentsArray: Expression[] = [
            factory.createPropertyAccessExpression(name, factory.createIdentifier("prototype")),
            factory.createStringLiteral(fullClassName)
        ];
        if (interfaces.length) {
            const elements: Expression[] = [];
            for (const value of interfaces) {
                elements.push(factory.createStringLiteral(value));
            }
            argumentsArray.push(factory.createArrayLiteralExpression(elements));
        }
        return factory.createCallExpression(
            context.getEmitHelperFactory().getUnscopedHelperName("__reflect"),
            /*typeArguments*/ undefined,
            argumentsArray
        );
    }
}