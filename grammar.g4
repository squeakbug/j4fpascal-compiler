grammar Delphi;

@lexer::namespace{DelphiGrammar}
@parser::namespace{DelphiGrammar}
//****************************
//section start
//****************************
file                         : program | library | unit | packageE
                             ;
//****************************
//section fileDefinition
//****************************

program                      : (programHead)? (usesFileClause)? block '.'
                             ;
programHead                  : 'program' namespaceName (programParmSeq)? ';'
                             ;
programParmSeq               : '(' (ident (',' ident)* )? ')'
                             ;
library                      : libraryHead (usesFileClause)? block '.'
                             ;
libraryHead                  : 'library' namespaceName (hintingDirective)* ';' 
                             ;
packageE                     : packageHead requiresClause (containsClause)? 'end' '.'
                             ;
packageHead                  : 'package' namespaceName ';'
                             ;
unit                         : unitHead unitInterface unitImplementation unitBlock '.'
                             ;
unitHead                     : 'unit' namespaceName (hintingDirective)* ';' 
                             ;
unitInterface                : 'interface' (usesClause)? (interfaceDecl)* 
                             ;
unitImplementation           : 'implementation' (usesClause)? (declSection)* 
                             ;
unitBlock                    : unitInitialization 'end'
                             | compoundStatement
                             | 'end'
                             ;
unitInitialization           : 'initialization' statementList (unitFinalization)?
                             ;
unitFinalization             : 'finalization' statementList
                             ;
//****************************
//section fileUsage
//****************************
containsClause               : 'contains' namespaceFileNameList
                             ;
requiresClause               : 'requires' namespaceNameList
                             ;
usesClause                   : 'uses' namespaceNameList 
                             ;
usesFileClause               : 'uses' namespaceFileNameList 
                             ;
namespaceFileNameList        : namespaceFileName (',' namespaceFileName)* ';' 
                             ;
namespaceFileName            : namespaceName ('in' QuotedString)? 
                             ;
namespaceNameList            : namespaceName (',' namespaceName)* ';' 
                             ;
//****************************
//section declaration
//****************************
block                        : (declSection)* (blockBody)?
                             ;
blockBody                    : compoundStatement
                             | assemblerStatement
                             ;
declSection                  : labelDeclSection
                             | constSection
                             | typeSection
                             | varSection
                             | exportedProcHeading
                             | methodDecl
                             | procDecl
                             | exportsSection
                             ;
interfaceDecl                : procDecl
                             | methodDecl
                             | typeSection
                             | varSection
                             | exportedProcHeading
                             | exportsSection
                             | constSection
                             ;
labelDeclSection             : 'label' label (',' label)* ';'
                             ;
constSection                 : constKey (constDeclaration)*  //CHANGED, erased one constDeclaration, for: "const {$include versioninfo.inc }"
                             ;
constKey                     : 'const'
                             | 'resourcestring'
                             ;
constDeclaration             : (customAttribute)? ident (':' typeDecl)? '=' constExpression (hintingDirective)* ';' 
                             ;
typeSection                  : 'type' typeDeclaration (typeDeclaration)* 
                             ;
typeDeclaration              : (customAttribute)? genericTypeIdent '=' typeDecl (hintingDirective)* ';' 
                             ;
varSection                   : varKey varDeclaration (varDeclaration)* 
                             ;
varKey                       : 'var'
                             | 'threadvar'
                             ;
// threadvar geen initializations alleen globaal
varDeclaration               : (customAttribute)? identListFlat ':' typeDecl (varValueSpec)? (hintingDirective)* ';' 
                             ;
varValueSpec                 : 'absolute' ident
                             | 'absolute' constExpression
                             | '=' constExpression
                             ;
exportsSection               : 'exports' ident exportItem (',' ident exportItem)* ';'
                             ;
exportItem                   : ('(' (formalParameterList)? ')')? (INDEX expression)? (NAME expression)? ('resident')?
                             ;
//****************************
//section type
//****************************
typeDecl                     : strucType
                             | pointerType
                             | stringType
                             | procedureType 
                             | variantType
                             | ('type')? typeId (genericPostfix)?
                             | simpleType
                             ;
strucType                    : ('packed')? strucTypePart 
                             ;
strucTypePart                : arrayType
                             | setType
                             | fileType
                             | classDecl
                             ;

arrayType                    :  'array' ('[' (arrayIndex)? (',' (arrayIndex)?)* ']')? 'of' arraySubType 
                                     //CHANGED we only need type info
                             ;

arrayIndex                   : typeId
                             | expression '..' expression
                             ;

arraySubType                 : 'const'
                             | typeDecl
                             ;
setType                      : 'set' 'of' typeDecl          //CHANGED we only need type info
                             ;
// set type alleen ordinal of subrange type
fileType                     : 'file' ('of' typeDecl)?
                             ;
pointerType                  : '^' typeDecl
                             | 'pointer'
                             ;
stringType                   : 'string' ('[' expression ']')? 
                             | ('type')? ANSISTRING (codePageNumber)?
                             ;
codePageNumber               : '(' intNum ')'
                             ;
procedureType                : methodType
                             | simpleProcedureType
                             | procedureReference
                             ;
methodType                   : procedureTypeHeading 'of' 'object'
                             ;
simpleProcedureType          : procedureTypeHeading ( (';')? callConventionNoSemi)?
                             ;
procedureReference           : 'reference' 'to' procedureTypeHeading
                             ;
procedureTypeHeading         : 'function' (formalParameterSection)? ':' (customAttribute)? typeDecl 
                             | 'procedure' (formalParameterSection)?
                             ;
variantType                  : 'variant' // SzJ TODO TEMP
                             ;
simpleType                   : ident
                             | subRangeType
                             | enumType
                             ;
subRangeType                 : constExpression ('..' constExpression)?
                             ;
enumType                     : '(' ident ('=' expression)? (',' ident ('=' expression)? )* ')'
                             ;
typeId                       : namespacedQualifiedIdent
                             ;
//****************************
//section generics
//****************************
genericTypeIdent             : qualifiedIdent (genericDefinition)?     //CHANGED we don't need <Type> data, it produced empty nodes
                             ;
genericDefinition            : simpleGenericDefinition
                             | constrainedGenericDefinition
                             ;
simpleGenericDefinition      : '<' ident (',' ident)* '>'
                             ;
constrainedGenericDefinition : '<' constrainedGeneric (';' constrainedGeneric)* '>'
                             ;
constrainedGeneric           : ident (':' genericConstraint (',' genericConstraint)*)?
                             ;
genericConstraint            : ident
                             | ( 'record' | 'class' | 'constructor' )
                             ;
genericPostfix               : '<' typeDecl (',' typeDecl)* '>'
                             ;
//****************************
//section class
//****************************
classDecl                    : classTypeTypeDecl
                             | classTypeDecl 
                             | classHelperDecl 
                             | interfaceTypeDecl 
                             | objectDecl 
                             | recordDecl 
                             | recordHelperDecl 
                             ;
classTypeTypeDecl            : 'class' 'of' typeId 
                             ;
classTypeDecl                : 'class' (classState)? (classParent)? (classItem)* 'end' 
                             | 'class' (classParent)? 
                             ;
classState                   : 'sealed'
                             | 'abstract'
                             ;
classParent                  : '(' genericTypeIdent (',' genericTypeIdent)* ')'    //CHANGEd from typeId to classParentId
                             ;
classItem                    : visibility
                             | classMethod
                             | classField
                             | classProperty
                             | constSection
                             | typeSection
                             | ('class')? varSection
                             ;
classHelperDecl              : 'class' 'helper' (classParent)? 'for' typeId (classHelperItem)* 'end' //CHANGED, we only need "for" class name
                             ;
classHelperItem              : visibility
                             | classMethod
                             | classProperty
                             | ('class')? varSection
                             ;
interfaceTypeDecl            : interfaceKey (classParent)? (interfaceGuid)? (interfaceItem)* 'end' 
                             | interfaceKey (classParent)? 
                             ;
interfaceKey                 : 'interface'
                             | 'dispinterface'
                             ;
interfaceGuid                : '[' QuotedString ']' 
                             ;
interfaceItem                : classMethod
                             | ('class')? classProperty
                             ;
objectDecl                   : 'object' (classParent)? (objectItem)* 'end' 
                             ;
objectItem                   : visibility
                             | classMethod
                             | classField
                             ;
recordDecl                   : simpleRecord
                             | variantRecord
                             ;
simpleRecord                 : 'record' (recordField)* (recordItem)* 'end' 
                             ;
variantRecord                : 'record' (recordField)* recordVariantSection 'end' 
                             ;
recordItem                   : visibility     //ADDED
                             | classMethod
                             | classProperty
                             | constSection
                             | typeSection
                             | recordField
                             | ('class')? varSection
                             ;
recordField                  : identList ':' typeDecl (hintingDirective)* (';')?  //CHANGED not needed ; at the end
                             ;
recordVariantField           : identList ':' typeDecl (hintingDirective)* (';') ?
                             ;
recordVariantSection         : 'case' (ident ':')? typeDecl 'of' (recordVariant | ';') (recordVariant | ';')*
                             ;
recordVariant                : constExpression (',' constExpression)* ':' '(' (recordVariantField)* ')'   //CHANGED to recordVariantField from recordField
                             ;
recordHelperDecl             : 'record' 'helper' 'for' typeId (recordHelperItem)* 'end'
                             ;
recordHelperItem             : visibility
                             | classMethod
                             | classProperty
                             ;
classMethod                  : (customAttribute)? ('class')? methodKey ident (genericDefinition)? (formalParameterSection)? ';' (methodDirective)* 
                             | (customAttribute)? ('class')? 'function' ident (genericDefinition)? (formalParameterSection)? ':' (customAttribute)? typeDecl ';' (methodDirective)*
                             | (customAttribute)? ('class')? 'operator' ident (genericDefinition)? (formalParameterSection)? ':' (customAttribute)? typeDecl ';'
                             ;                             
classField                   : (customAttribute)? identList ':' typeDecl ';' (hintingDirective)* 
                             ;
classProperty                : (customAttribute)? ('class')? 'property' ident (classPropertyArray)? (':' genericTypeIdent)? (classPropertyIndex)? (classPropertySpecifier)* ';' (classPropertyEndSpecifier)*
                              // CHANGED added (classPropertySpecifier)* at end for "default;"
                              // CHANGEDD to genericTypeIdent for "property QueryBuilder : IQueryBuilder<GenericRecord>"
                             ;
classPropertyArray           : '[' formalParameterList ']'
                             ;
classPropertyIndex           : 'index' expression (';')?  //CHANGED to (';')?
                             ;
classPropertySpecifier       : classPropertyReadWrite   //CHANGED removed ';'
                             | classPropertyDispInterface
                             | STORED expression
                             | 'default' expression
                             | ( 'default'                // for array properties only (1 per class)
                             | 'nodefault' )
                             | IMPLEMENTS typeId
                             ;
classPropertyEndSpecifier    : STORED expression ';'    //ADDED used in classProperty at end
                             | 'default' expression ';'
                             | 'default' ';'             
                             | 'nodefault' ';'
                             ;

classPropertyReadWrite       : 'read' qualifiedIdent ('[' expression ']')?  // Waarom qualified ident???  //ADDED []
                             | 'write' qualifiedIdent ('[' expression ']')? //ADDED []
                             ;
classPropertyDispInterface   : 'readonly' ';'
                             | 'writeonly' ';'
                             | dispIDDirective
                             ;
visibility                   : (STRICT)? 'protected' 
                             | (STRICT)? 'private'
                             | ( 'public'
                             | 'published' 
                             | 'automated' )     // win32 deprecated
                             ;
//****************************
//section procedure
//****************************
exportedProcHeading          : 'procedure' ident (formalParameterSection)? ':' (customAttribute)? typeDecl ';' (functionDirective)*
                             | 'function' ident (formalParameterSection)? ';' (functionDirective)*
                             ;
methodDecl                   : methodDeclHeading ';' (methodDirective)* (methodBody)? 
                             ;
methodDeclHeading            : (customAttribute)? ('class')?  methodKey methodName (formalParameterSection)?
                             | (customAttribute)? ('class')? 'function' methodName (formalParameterSection)? (':' (customAttribute)? typeDecl)?
                             | (customAttribute)? 'class' 'operator' methodName (formalParameterSection)? (':' (customAttribute)? typeDecl)?
                             ;              
methodKey                    : 'procedure'
                             | 'constructor'
                             | 'destructor'
                             ;
methodName                   : ident (genericDefinition)? ('.' ident (genericDefinition)?)? '.' ident (genericDefinition)?
                             ;                             
procDecl                     : procDeclHeading ';' (functionDirective)* (procBody)?     //CHANGED
                             ;
procDeclHeading              : (customAttribute)? 'procedure' ident (formalParameterSection)?             //CHANGED
                             | (customAttribute)? 'function' ident (formalParameterSection)? ':' typeDecl
                             ;
formalParameterSection       : '(' (formalParameterList)? ')' 
                             ;
formalParameterList          : formalParameter (';' formalParameter)* 
                             ;
formalParameter              : //(customAttribute)? 
                               (parmType)? identListFlat (':' typeDecl)? ('=' expression)? 
               //expressions was cut out, beacause we dont have to know default variable values; they were causing troubles with DelphiCodeAnalyser
                             ;
parmType                     : 'const'
                             | 'var'
                             | 'out'
                             ;
methodBody                   : block ';' 
                             ;
procBody                     : 'forward' ';' (functionDirective)*   // CHECKEN ; en directive plaats!
                             | 'external' ('name' expression | 'index' expression)* (functionDirective)* // CHECKEN directive plaats
                             | block ';'
                             ;
//****************************
//section customAttributes
//****************************
customAttribute              : 'abekat' //customAttributeList
                             ;
customAttributeList          : (customAttributeDecl)*
                             ;
customAttributeDecl          : '[' namespacedQualifiedIdent ('(' (expressionList)? ')')? ']'  
                             ;                             

//****************************
//section expression
//****************************
expression                   : anonymousExpression 
                             | simpleExpression (relOp simpleExpression)? ('=' expression)?   //CHANGED, added expression for: "if( functionCall(x, 7+66) = true ) then" syntax
                             ;                           
anonymousExpression          : 'procedure' (formalParameterSection)? block
                             | 'function' (formalParameterSection)? ':' typeDecl block
                             ;
simpleExpression             : signedFactor (operator signedFactor)*
                             ;
signedFactor                 : ('+' | '-')? factor
                             ;
factor                       : '@' factor
                             | DOUBLEAT factor       // used to get address of proc var
                             | 'not' factor
                             | '^' ident           // geeft volgnummer van letter
                             | unsignedConstant
                             | TkAsmHexNum          // Alleen in asm statement
                             | bool_
                             | '(' expression ')' ('^')? ('.' expression)?        //CHANGED, added  ('^')? ('.' qualifiedIdent)?
                             | setSection
                             | designator
                             | typeId '(' expression ')'
                             ;
unsignedConstant             : intNum | realNum | stringFactor | 'nil'
                             ;
bool_                        : 'true' | 'false'
                             ;
stringFactor                 : ControlString (QuotedString ControlString)* (QuotedString)?
                             | QuotedString (ControlString QuotedString)* (ControlString)?
                             ;
setSection                   : '[' (expression ((',' | '..') expression)*)? ']'
                             ;

designator                   : ('inherited')? ( (namespacedQualifiedIdent | typeId) )? (designatorItem)*
                             ;
designatorItem               : '^'
                             | ('.' | '@') ident              //CHANGED added '@'
                             | ('<' genericTypeIdent (',' genericTypeIdent)* '>')       //ADDED for proc<sth, sth>.foo;
                             | '[' expressionList ']'
                             | '(' parameterList ')'
                             ;
parameterList                : (actualParameter (',' actualParameter)*)? 
                             ;
actualParameter              : expression (colonConstruct)?
                             ;
expressionList               : expression (',' expression)*
                             ;
colonConstruct               : ':' expression (':' expression)?
                             ;
// Alleen voor Write/WriteLn.
operator                     : '+'
                             | '-'
                             | 'or'
                             | 'xor'
                             | '*'
                             | '/'
                             | 'div'
                             | 'mod'
                             | 'and'
                             | 'shl'
                             | 'shr'
                             | 'as'
                             ;
relOp                        : '<'
                             | '>'
                             | '<='
                             | '>='
                             | '<>'
                             | '='
                             | 'in'
                             | 'is'
                             ;
//****************************
//section statement
//****************************

statement                    : label ':' unlabeledStatement
                             | unlabeledStatement
                             ;

unlabeledStatement           : ifStatement
                             | caseStatement
                             | repeatStatement
                             | whileStatement
                             | forStatement
                             | withStatement
                             | tryStatement
                             | raiseStatement
                             | assemblerStatement
                             | compoundStatement
                             | simpleStatement
                             ;
ifStatement                  : 'if' expression 'then' statement ('else' statement)? 
                             ;
caseStatement                : 'case' expression 'of' (caseItem)* ('else' statementList (';')?)? 'end'
                             ;
caseItem                     : caseLabel (',' caseLabel)* ':' statement (';')? // checken of ; sep of scheider is
                             ;
caseLabel                    : expression ('..' expression)?
                             ;
repeatStatement              : 'repeat' (statementList)? 'until' expression
                             ;
whileStatement               : 'while' expression 'do' statement
                             ;
forStatement                 : 'for' designator ':=' expression 'to' expression 'do' statement
                             | 'for' designator ':=' expression 'downto' expression 'do' statement
                             | 'for' designator 'in' expression 'do' statement
                             ;
withStatement                : 'with' withItem 'do' statement
                             ;
withItem                     : designator 'as' designator       //ADDED
                             | designator (',' designator)*
                             ;
compoundStatement            : 'begin' (statementList)? 'end' 
                             ;
statementList                : (statement)? (';' (statement)?)*
                             ;
simpleStatement              : designator ':=' expression
                             | designator // call
                             | gotoStatement
                             ;
gotoStatement                : 'goto' label
                             | 'exit' ('(' expression ')')?   
                             | ( 'break'                          
                             | 'continue' )
                             ;
//****************************
//section constExpression
//****************************
constExpression              : '(' recordConstExpression (';' recordConstExpression)* ')' //CHANGED reversed order
                             | '(' constExpression (',' constExpression)* ')'
                             | expression
                             ;
recordConstExpression        : ident ':' constExpression
                             ;
//****************************
//section exceptionStatement
//****************************
tryStatement                 : 'try' (statementList)? 'except' handlerList 'end'  
                             | 'try' (statementList)? 'finally' (statementList)? 'end'
                             ;
handlerList                  : (handler)* ('else' statementList)?
                             | statementList
                             ;
handler                      : 'on' (handlerIdent)? typeId 'do' handlerStatement  //CHANGED - ; is not required ; handlerIdent not required, example:  "on einvalidoperation do;"
                             ;
handlerIdent                 : ident ':'
                             ;
handlerStatement             : statement (';')?
                             | ';'
                             ;
raiseStatement               : 'raise' (designator)? (AT designator)? // CHECKEN!
                             ;           
//****************************
//section AssemblerStatement
//****************************
assemblerStatement           : 'asm' ~('end')* 'end'    //ADDED we don't realy care about assembler statements, since they don't contribute to
                             ;                //any measure, just skip, allow all
//****************************
//section directive
//****************************
methodDirective              : reintroduceDirective         // 1
                             | overloadDirective            // 2
                             | bindingDirective             // 3
                             | abstractDirective            // 3 virtual;
                             | inlineDirective              // 4 niet virtual or dynamic
                             | callConvention               // 4
                             | hintingDirective ';'       // 4 (niet abstract)
                             | oldCallConventionDirective   // 1
                             | dispIDDirective
                             ;
functionDirective            : overloadDirective          // 1
                             | inlineDirective            // 1
                             | callConvention             // 1
                             | oldCallConventionDirective // 1
                             | hintingDirective ';'      // 1
                             | (callConventionNoSemi)? externalDirective          // 1
                             | 'unsafe' ';'              // 1 .net?
                             ;
reintroduceDirective         : 'reintroduce' ';'
                             ;
overloadDirective            : 'overload' (';')?    //CHANGE ; not needed
                             ;
bindingDirective             : 'message' expression ';'
                             | 'static' ';'
                             | 'dynamic' ';'
                             | 'override' ';'
                             | 'virtual' ';'
                             ;
abstractDirective            : 'abstract' ';'
                             | 'final' ';'
                             ;
inlineDirective              : 'inline' ';'
                             | 'assembler' ';' // deprecated
                             ;
callConvention               : 'cdecl' ';'    //
                             | 'pascal' ';'   //
                             | 'register' ';' //
                             | 'safecall' ';' //
                             | 'stdcall' ';'  //
                             | 'export' ';'   // deprecated
                             ;
callConventionNoSemi         : 'cdecl'    //    //ADDED for procedureType error fixing, without ';' at the end
                             | 'pascal'   //
                             | 'register' //
                             | 'safecall' //
                             | 'stdcall'  //
                             | 'export'   // deprecated
                             ;
oldCallConventionDirective   : 'far' ';'      // deprecated
                             | 'local' ';'    // niet in windows maakt functie niet exporteerbaar
                             | 'near' ';'     // deprecated
                             ;
hintingDirective             : 'deprecated' (stringFactor)?
                             | ( 'experimental'  // added 2006
                             | 'platform'
                             | 'library' )
                             ;
externalDirective            : 'varargs' ';'   // alleen bij external cdecl
                             | 'external' ';'
                             | 'external' constExpression (externalSpecifier)* ';' // expression : dll name
                             ;
externalSpecifier            : 'name' constExpression
                             | 'index' constExpression   // specific to a platform
                             ;
dispIDDirective              : 'dispid' expression ';'
                             ;
//****************************
////section general
//****************************
ident                        : TkIdentifier
                             | AMBER TkIdentifier
                             | usedKeywordsAsNames
                             ;                 
usedKeywordsAsNames          : (NAME | READONLY | ADD | AT | MESSAGE | POINTER | INDEX | DEFAULT | STRING | CONTINUE)
                             | (READ | WRITE | REGISTER | VARIANT | OPERATOR | REMOVE | LOCAL | REFERENCE | CONTAINS | FINAL)
                             | (BREAK | EXIT | STRICT | OUT | OBJECT | EXPORT | ANSISTRING | IMPLEMENTS | STORED)
                             ;                           
identList                    : ident (',' ident)* 
                             ;
identListFlat                : ident (',' ident)*    //ADDED used in formalParemeter
                             ;                                                          
label                        : ( TkIdentifier | TkIntNum | TkHexNum ) | usedKeywordsAsNames 
                             ;
intNum                       : TkIntNum
                             | TkHexNum
                             ;                             
realNum                      : TkRealNum
                             ;                             
namespacedQualifiedIdent     : (namespaceName '.')? qualifiedIdent
                             ;
namespaceName                : ident ('.' ident)*
                             ;
qualifiedIdent               :  (ident '.')*  ident   //must stay the way it is, with '.' for proper class method identyfication
                             ;
                                   
// KEYWORDS
ABSOLUTE          : 'absolute'       ;
ABSTRACT          : 'abstract'       ;
ADD               : 'add'            ;
AND               : 'and'            ;
ANSISTRING        : 'ansistring'     ;
ARRAY             : 'array'          ;
AS                : 'as'             ;
ASM               : 'asm'            ;
ASSEMBLER         : 'assembler'      ;
ASSEMBLY          : 'assembly'       ;
AT                : 'at'             ;
AUTOMATED         : 'automated'      ;
BEGIN             : 'begin'          ;
BREAK             : 'break'          ;
CASE              : 'case'           ;
CDECL             : 'cdecl'          ;
CLASS             : 'class'          ;
CONST             : 'const'          ;
CONSTRUCTOR       : 'constructor'    ;
CONTAINS          : 'contains'       ;
CONTINUE          : 'continue'       ;
DEFAULT           : 'default'        ;
DEPRECATED        : 'deprecated'     ;
DESTRUCTOR        : 'destructor'     ;
DISPID            : 'dispid'         ;
DISPINTERFACE     : 'dispinterface'  ;
DIV               : 'div'            ;
DO                : 'do'             ;
DOWNTO            : 'downto'         ;
DQ                : 'dq'             ;
DW                : 'dw'             ;
DYNAMIC           : 'dynamic'        ;
ELSE              : 'else'           ;
END               : 'end'            ;
EXCEPT            : 'except'         ;
EXIT              : 'exit'           ;
EXPERIMENTAL      : 'experimental'   ;
EXPORT            : 'export'         ;
EXPORTS           : 'exports'        ;
EXTERNAL          : 'external'       ;
FAR               : 'far'            ;
FILE              : 'file'           ;
FINAL             : 'final'          ;
FINALIZATION      : 'finalization'   ;
FINALLY           : 'finally'        ;
FOR               : 'for'            ;
FORWARD           : 'forward'        ;
FUNCTION          : 'function'       ;
GOTO              : 'goto'           ;
HELPER            : 'helper'         ;
IF                : 'if'             ;
IMPLEMENTATION    : 'implementation' ;
IMPLEMENTS        : 'implements'     ;
IN                : 'in'             ;
INDEX             : 'index'          ;
INHERITED         : 'inherited'      ;
INITIALIZATION    : 'initialization' ;
INLINE            : 'inline'         ;
INTERFACE         : 'interface'      ;
IS                : 'is'             ;
LABEL             : 'label'          ;
LIBRARY           : 'library'        ;
LOCAL             : 'local'          ;
MESSAGE           : 'message'        ;
MOD               : 'mod'            ;
NAME              : 'name'           ;
NEAR              : 'near'           ;
NIL               : 'nil'            ;
NODEFAULT         : 'nodefault'      ;
NOT               : 'not'            ;
OBJECT            : 'object'         ;
OF                : 'of'             ;
ON                : 'on'             ;
OPERATOR          : 'operator'       ;
OR                : 'or'             ;
OUT               : 'out'            ;
OVERLOAD          : 'overload'       ;
OVERRIDE          : 'override'       ;
PACKAGE           : 'package'        ;
PACKED            : 'packed'         ;
PASCAL            : 'pascal'         ;
PLATFORM          : 'platform'       ;
POINTER           : 'pointer'        ;
PRIVATE           : 'private'        ;
PROCEDURE         : 'procedure'      ;
PROGRAM           : 'program'        ;
PROPERTY          : 'property'       ;
PROTECTED         : 'protected'      ;
PUBLIC            : 'public'         ;
PUBLISHED         : 'published'      ;
RAISE             : 'raise'          ;
READ              : 'read'           ;
READONLY          : 'readonly'       ;
RECORD            : 'record'         ;
REFERENCE         : 'reference'      ;
REGISTER          : 'register'       ;
REINTRODUCE       : 'reintroduce'    ;
REMOVE            : 'remove'         ;
REPEAT            : 'repeat'         ;
REQUIRES          : 'requires'       ;
RESIDENT          : 'resident'       ;
RESOURCESTRING    : 'resourcestring' ;
SAFECALL          : 'safecall'       ;
SEALED            : 'sealed'         ;
SET               : 'set'            ;
SHL               : 'shl'            ;
SHR               : 'shr'            ;
STATIC            : 'static'         ;
STDCALL           : 'stdcall'        ;
STORED            : 'stored'         ;
STRICT            : 'strict'         ;
STRING            : 'string'         ;
THEN              : 'then'           ;
THREADVAR         : 'threadvar'      ;
TO                : 'to'             ;
TRY               : 'try'            ;
TYPE              : 'type'           ;
UNIT              : 'unit'           ;
UNSAFE            : 'unsafe'         ;
UNTIL             : 'until'          ;
USES              : 'uses'           ;
VAR               : 'var'            ;
VARARGS           : 'varargs'        ;
VARIANT           : 'variant'        ;
VIRTUAL           : 'virtual'        ;
WHILE             : 'while'          ;
WITH              : 'with'           ;
WRITE             : 'write'          ;
WRITEONLY         : 'writeonly'      ;
XOR               : 'xor'            ;
FALSE             : 'false'          ;
TRUE              : 'true'           ;

//----------------------------------------------------------------------------
// OPERATORS
//----------------------------------------------------------------------------
PLUS              : '+'   ;
MINUS             : '-'   ;
STAR              : '*'   ;
SLASH             : '/'   ;
ASSIGN            : ':='  ;
COMMA             : ','   ;
SEMI              : ';'   ;
COLON             : ':'   ;
EQUAL             : '='   ;
NOT_EQUAL         : '<>'  ;
LT                : '<'   ;
LE                : '<='  ;
GE                : '>='  ;
GT                : '>'   ;
LPAREN            : '('   ;
RPAREN            : ')'   ;
LBRACK            : '['   ; // line_tab[line]
LBRACK2           : '(.'  ; // line_tab(.line.)
RBRACK            : ']'   ;
RBRACK2           : '.)'  ;
POINTER2          : '^'   ;
AT2               : '@'   ;
DOT               : '.'   ;// ('.' {$setType(DOTDOT);})?  ;
DOTDOT            : '..'  ;
LCURLY            : '{'   ;
RCURLY            : '}'   ;     

AMBER             : '&'   ;
DOUBLEAT          : '@@'  ;

//****************************
//section token
//****************************
TkGlobalFunction        : 'FUNCTION_GLOBAL'
                        ;
TkFunctionName          : 'FUNCTION_NAME'
                        ;
TkFunctionArgs          : 'FUNCTION_ARGS'
                        ;
TkFunctionBody          : 'FUNCTION_BODY'
                        ;
TkFunctionReturn        : 'FUNCTION_RETURN'
                        ;
TkCustomAttribute       : 'CUSTOM_ATTRIBUTE'
                        ;
TkCustomAttributeArgs   : 'CUSTOM_ATTRIBUTE_ARGS'
                        ;
TkNewType               : 'NEW_TYPE'
                        ;
TkClass                 : 'CLASS'
                        ;
TkRecord                : 'RECORD_TYPE'
                        ;
TkRecordHelper          : 'RECORD_HELPER'
                        ;
TkInterface             : 'INTERFACE_TYPE'
                        ;
TkObject                : 'OBJECT_TYPE'
                        ;
TkClassOfType           : 'CLASS_OF_TYPE'
                        ;
TkVariableType          : 'VARIABLE_TYPE'
                        ;
TkVariableIdents        : 'VARIABLE_IDENTS'
                        ;
TkVariableParam         : 'VARIABLE_PARAM'
                        ;
TkGuid                  : 'INTERFACE_GUID'
                        ;
TkClassParents          : 'CLASS_PARENTS'
                        ;
TkClassField            : 'CLASS_FIELD'
                        ;
TkAnonymousExpression   : 'ANONYMOUS_EXPRESSION'
                        ;
TkIdentifier            : (Alpha | '_') (Alpha | Digit | '_')*
                        ;  
TkIntNum                : Digitseq
                        ;
TkRealNum               : Digitseq ('.' Digitseq)? (('e'|'E') ('+'|'-')? Digitseq)?  //CHANGED
                        ;
TkHexNum                : '$' Hexdigitseq
                        ;
TkAsmHexNum             : Hexdigitseq ('h'|'H')
                        ;
TkAsmHexLabel           : Hexdigitseq ':'
                        ;
QuotedString            : '\'' ('\'\'' | ~('\''))* '\''   //taken from PASCAL grammar
                        ;
ControlString           : Controlchar (Controlchar)*
                        ;
                        
fragment                
Controlchar             : '#' Digitseq
                        | '#' '$' Hexdigitseq
                        ;
fragment                
Alpha                   : 'a'..'z'
                        | 'A'..'Z'
                        | '\u0080'..'\uFFFE' ~('\uFEFF') //ADDED unicode support
                        ;
fragment                
Digit                   : '0'..'9'
                        ;
fragment                
Digitseq                : Digit (Digit)*
                        ;
fragment                
Hexdigit                : Digit | 'a'..'f' | 'A'..'F'
                        ;
Hexdigitseq             : Hexdigit (Hexdigit)*
                        ;
COMMENT                 :  ( '//' ~('\n'|'\r')* '\r'? '\n'           
                        |  '(*' .*? '*)'  
                        |  '{' .*? '}')    -> skip
                        ;                 
WS                      : (' '|'\t'|'\r'|'\n'|'\f')+ -> skip
                        ;
UnicodeBOM              : '\uFEFF' -> skip 
                        ;                             