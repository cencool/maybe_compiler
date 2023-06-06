(*
Grammar:                               Syntax nodes:
prg -> block prg                       prg.n = PrgSeq(Nodes: TSyntaxNode...)
      | block                          prg.n = PrgSeq(Nodes: TSyntaxNode...)
block ->   { decls statements }        block.n = statements.n

statements -> block                    statements.n = block.n
            | assignment ; statements  statements.n = Seq(Nodes: TSyntaxNode...)
            | expr ; statements        statements.n = Seq(Nodes: TSyntaxNode...)
            | @                        statements.n = Seq(Nodes: TSyntaxNode...)

decls -> decl decls                    decls.n = nil
        | @                            decls.n = nil

decl -> TYPENAME ID ;                  decl.n = nil

assignment -> ID = expr                assignment.n =  Assign(LeftSide, RightSide)

bool -> bool_term bool_rest

bool_term -> expr

bool_rest ->  || bool_term bool_rest
            | && bool_term bool_rest
            | @

expr -> term expr_rest                 expr.n =  Expr(Term();Expr(); nil)

expr_rest -> + term expr_rest          expr_rest.n = Expr(Term();Expr(); Lookahead)
           | - term expr_rest          expr_rest.n =  Expr(Term();Expr(); Lookahead)
           | @

term -> factor term_rest               term.n =  Term(Factor(); Term(); nil)

term_rest -> * factor term_rest        term_rest.n =  Term(Factor(); Term(); Lookahead)
           | / factor term_rest        term_rest.n =  Term(Factor(); Term(); Lookahead)
           | @                         term_rest.n = nil

factor -> ID                           factor.n = Factor (Lookahead)
        | NUMBER                       factor.n =  Factor (Lookahead)
        | ( bool)                      factor.n = expr.n


@ means empty production

*)
{ #todo : upravit gramatiku pre bool podla realneho kodu ... }
{ #done : zda sa ze prepis do postscript notacie pre napr. 1/2*3 nie je dobre }
{ #todo : nejako cudne teraz vyzera syntax tree, treba zmenit }
{ #todo : nespravne vyhodnotenie ak x= 3/2 ; y = 2+x }

unit Parser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  Lexer,
  Contnrs,
  ComCtrls;

type
  TValueType = (vtNUM, vtBOOL, vtUNDEFINED);

  { TExprValue }

  TExprValue = class
  public
    ValueType: TValueType;
    Value: string;
    constructor Create(AType: TValueType; AValue: string = '');
  end;

  { TParseNode }

  TParseNode = class
    Name: string;
    ChildrenList: TList;
    constructor Create();
    destructor Destroy; override;
    procedure Link(Node: TParseNode);
    procedure AddChildWithText(TextToAdd: string);
  end;

  { TSyntaxNode }

  TSyntaxNode = class
  private
    FDisplayText: string;
    procedure SetDisplayText(AValue: string);
  public
    EvalData: TExprValue;
    { children list purpose is to unify access to node childrens if different member names used }
    ChildrenList: TList;
    procedure Gen; virtual;
    function Eval(AInputValue: TExprValue = nil): TExprValue; virtual;
    destructor Destroy; override;
    function GetDisplayText: string; virtual;
    function PositionString: string;

    property DisplayText: string read GetDisplayText write SetDisplayText;
  end;

  { TExpr }

  TExpr = class(TSyntaxNode)
    Expr: TSyntaxNode;
    Term: TSyntaxNode;
    OpTokenTag: TTokenTag;
    OpTokenLexeme: string;
    constructor Create(ATerm: TSyntaxNode; AExpr: TSyntaxNode; AOpToken: TToken = nil);
    procedure Gen; override;
    function Eval(AInputValue: TExprValue): TExprValue; override;
    function DetermineType: TValueType;
  end;

  { TTermNode }

  TTerm = class(TSyntaxNode)
    Factor: TSyntaxNode;
    Term: TSyntaxNode;
    TokenTag: TTokenTag;
    TokenLexeme: string;
    constructor Create(AFactor: TSyntaxNode; ATerm: TSyntaxNode; AOpToken: TToken = nil);
    procedure Gen; override;
    function Eval(AInputValue: TExprValue = nil): TExprValue; override;
    function DetermineType: TValueType;
  end;

  { TPrgSeq }

  TPrgSeq = class(TSyntaxNode)
    FPrgSeq: TList;
    constructor Create;
    procedure Link(ANodeToLink: TSyntaxNode);
    procedure Gen; override;
  end;

  { TSequence }

  TSeq = class(TSyntaxNode)
    FExprSeq: TList;
    constructor Create;
    procedure Link(ANodeToLink: TSyntaxNode);
    procedure Gen; override;
  end;

  { TFactor }

  TFactor = class(TSyntaxNode)
    Tag: TTokenTag;
    Lexeme: string;
    constructor Create(ATag: TTokenTag; ALexeme: string);
    procedure Gen; override;
    function Eval(AInputValue: TExprValue = nil): TExprValue; override;
  end;

  { TSemicolon }

  TSemicolon = class(TSyntaxNode)
    procedure Gen; override;
    constructor Create;
  end;

  { TAssign }

  TAssign = class(TSyntaxNode)
    { #todo co vlastne chcem aby robil ? }
    LeftSide: string;
    RightSide: TExpr;
    constructor Create(ALeftSide: string; ARightSide: TExpr = nil);
    procedure Gen; override;

  end;

  { TBool }

  TBool = class(TSyntaxNode)
    BoolTerm: TSyntaxNode;
    Bool: TSyntaxNode;
    OpTokenTag: TTokenTag;
    TokenLexeme: string;
    constructor Create(ABoolTerm: TSyntaxNode; ABool: TSyntaxNode; AOpToken: TToken = nil);
    procedure Gen; override;
    function DetermineType: TValueType;
    function Eval(AInputValue: TExprValue = nil): TExprValue; override;

  end;

  { TNodes }

  TNodes = class
    ParseNode: TParseNode;
    SyntaxNode: TSyntaxNode;
    constructor Create;
    constructor CreateAssign(var ParseVar: TParseNode; var SyntaxVar: TSyntaxNode);
    constructor CreateAssignParse(var ParseVar: TParseNode);
    destructor Destroy; override;
  end;

  { TSymbol }

  TSymbol = class
    EvalData: TExprValue;
    //FSymbolType: string;
    //SymbolValueText: string;
    IsAssigned: boolean;
    Position: TPosition;
    constructor Create;
  end;

  { TSymbolTable }

  TSymbolTable = class
    ParentTable: TSymbolTable;
    Symbols: TFPObjectHashTable;
    constructor Create(Parent: TSymbolTable);
    destructor Destroy; override;
    procedure Add(ALexeme: string; ASymbol: TSymbol);
    function GetType(ALexeme: string): TValueType;
    procedure PrintSymbols;
  end;

  { TParser }

  TParser = class
  public
    NewNode: TParseNode;
    ParseRoot: TParseNode;
    SyntaxRoot: TSyntaxNode;
    SymbolTableCurrent: TSymbolTable;
    { #done : add freeing objects }

    destructor Destroy; override;
    procedure CreateNewNode;
    procedure CreateNewNodeAndLink(Node: TParseNode);
    procedure parse(FileNameToParse: string);
    procedure parseManual(AMemoLines: TStrings);
    function prg(): TNodes;
    function block(): TNodes;
    function decls(): TNodes;
    function decl(): TNodes;
    function assignment(): TNodes;
    function statements(): TNodes;
    function bool(): TNodes;
    function bool_term(): TNodes;
    function bool_rest(): TNodes;
    function expr(): TNodes;
    function term(): TNodes;
    function factor(): TNodes;
    function expr_rest(): TNodes;
    function term_rest(): TNodes;
    procedure PrintParseTree(Node: TParseNode; SpaceCount: word = 0);
    procedure ShowParseTree(AParseNode: TParseNode; ATreeView: TTreeView);
    procedure PrintSyntaxTree(ANode: TSyntaxNode; SpaceCount: word = 0);
    function PositionString: string;
    procedure FreeParseTree(Node: TParseNode);
    function FindSymbol(AIdentifier: string): TSymbol;
  end;


implementation

uses
  SysUtils;

var
  Lex: TLexer;

{ TSymbol }

constructor TSymbol.Create;
begin
  IsAssigned := False;
end;

{ TExprValue }


constructor TExprValue.Create(AType: TValueType; AValue: string = '');
begin
  ValueType := AType;
  Value := AValue;
end;


{ TBool }

constructor TBool.Create(ABoolTerm: TSyntaxNode; ABool: TSyntaxNode; AOpToken: TToken);
begin
  BoolTerm := ABoolTerm;
  Bool := ABool;
  EvalData := TExprValue.Create(DetermineType); // syntax node type given by BoolTerm
  if AOpToken <> nil then
  begin
    if (AOpToken.Tag = ttOR) or (AOPToken.Tag = ttAND) then
    begin
      if (EvalData.ValueType <> vtBOOL) then
        raise Exception.Create(PositionString + 'Non Bool operand type in Bool Expression');
      OpTokenTag := AOpToken.Tag;
      TokenLexeme := AOpToken.Lexeme;
      DisplayText := TokenLexeme;
    end
    else
      raise  Exception.Create(PositionString + AOpToken.Lexeme +
        ' token passed to Bool Syntax Node');
  end
  else
  begin
    {type of node for non-operation case is given by BoolTerm type }
    OpTokenTag := ttNONE;
    DisplayText := 'Bool';
  end;
  ChildrenList := TList.Create;
  if EvalData.ValueType = vtUNDEFINED then raise Exception.Create('Type mismatch in TBool');
  ChildrenList.Add(BoolTerm);
  ChildrenList.Add(Bool);
  FreeAndNil(AOpToken);
end;

procedure TBool.Gen;
begin
  if BoolTerm <> nil then BoolTerm.Gen;
  if OpTokenTag <> ttNONE then Write(' ' + TokenLexeme + ' ');
  // for correct postfix order
  if Bool <> nil then Bool.Gen;
end;



function TBool.DetermineType: TValueType;
begin
  Result := vtUNDEFINED;
  if BoolTerm <> nil then Result := BoolTerm.EvalData.ValueType;
  if (Bool <> nil) and (Bool.EvalData.ValueType <> BoolTerm.EvalData.ValueType) then
    Result := vtUNDEFINED;
end;

function TBool.Eval(AInputValue: TExprValue): TExprValue;
begin
  Result := BoolTerm.Eval();
  case OpTokenTag of
    ttOR: begin
      Result.Value := BoolToStr(StrToBool(AInputValue.Value) or StrToBool(Result.Value), True);
    end;
    ttAND: begin
      Result.Value := BoolToStr(StrToBool(AInputValue.Value) and StrToBool(Result.Value), True);
    end;
  end;
  if Bool <> nil then
  begin
    Result := Bool.Eval(Result);
  end;

end;

{ TAssign }

constructor TAssign.Create(ALeftSide: string; ARightSide: TExpr = nil);
begin
  LeftSide := ALeftSide;
  RightSide := ARightSide;
  DisplayText := 'Assign';
  if ARightSide <> nil then
  begin
    ChildrenList := TList.Create;
    ChildrenList.Add(ARightSide);
  end;
end;

procedure TAssign.Gen;
begin
  Write(' ' + LeftSide + ' = ');
  RightSide.Gen;
  Write(';' + LineEnding);

end;


{ TSemicolon }

procedure TSemicolon.Gen;
begin
  WriteLn(';');
end;

constructor TSemicolon.Create;
begin
  DisplayText := ';';
end;

{ TPrgSeq }

constructor TPrgSeq.Create;
begin
  FPrgSeq := TList.Create;
  DisplayText := 'PrgSeq';
  ChildrenList := FPrgSeq;
end;

procedure TPrgSeq.Link(ANodeToLink: TSyntaxNode);
begin
  FPrgSeq.add(ANodeToLink);
end;

procedure TPrgSeq.Gen;
var
  i: integer;
begin
  for i := 0 to FPrgSeq.Count - 1 do
  begin
    TSyntaxNode(FPrgSeq.Items[i]).Gen;
  end;
end;

{ TFactor }

constructor TFactor.Create(ATag: TTokenTag; ALexeme: string);
begin
  Tag := ATag;
  Lexeme := ALexeme;
end;

procedure TFactor.Gen;
begin
  Write(' ' + Lexeme + ' ');
end;

function TFactor.Eval(AInputValue: TExprValue = nil): TExprValue;
begin
  // should return copy not reference otherwise will modify itself during run
  Result := TExprValue.Create(EvalData.ValueType, EvalData.Value);
end;

{ TSequence }

constructor TSeq.Create;
begin
  FExprSeq := TList.Create;
  DisplayText := 'Seq';
  ChildrenList := FExprSeq;
end;

procedure TSeq.Link(ANodeToLink: TSyntaxNode);
begin
  FExprSeq.Add(ANodeToLink);
end;

procedure TSeq.Gen;
var
  i: integer;
begin
  for i := 0 to FExprSeq.Count - 1 do
  begin
    TSyntaxNode(FExprSeq.Items[i]).Gen;
  end;

end;

{ TExpr }

constructor TExpr.Create(ATerm: TSyntaxNode; AExpr: TSyntaxNode; AOpToken: TToken = nil);
begin
  Term := ATerm;
  Expr := AExpr;
  EvalData := TExprValue.Create(DetermineType);

  if AOpToken <> nil then
  begin
    if (AOpToken.Tag = ttPLUS) or (AOPToken.Tag = ttMINUS) then
    begin
      if (EvalData.ValueType <> vtNUM) then
        raise Exception.Create('Non NUM type operand used in Expr');
      OpTokenTag := AOpToken.Tag;
      OpTokenLexeme := AOpToken.Lexeme;
      DisplayText := OpTokenLexeme;
    end
    else
      raise  Exception.Create(AOpToken.Lexeme + ' token passed to Expr Syntax Node');
  end
  else
  begin
    OpTokenTag := ttNONE;
    DisplayText := 'Expr';
  end;
  ChildrenList := TList.Create;
  if EvalData.ValueType = vtUNDEFINED then raise Exception.Create('Type mismatch in TExpr');
  ChildrenList.Add(Term);
  ChildrenList.Add(Expr);
  FreeAndNil(AOpToken);
end;

procedure TExpr.Gen;
begin
  if Term <> nil then Term.Gen;
  if OpTokenTag <> ttNONE then Write(' ' + OpTokenLexeme + ' ');
  // for correct postfix order
  if Expr <> nil then Expr.Gen;

end;

function TExpr.Eval(AInputValue: TExprValue): TExprValue;
begin
  { building result of expression based on available components of exp }
  Result := Term.Eval();
  case OpTokenTag of
    ttPLUS: begin
      Result.Value := FloatToStr(StrToFloat(AInputValue.Value) + StrToFloat(Result.Value));
    end;
    ttMINUS: begin
      Result.Value := FloatToStr(StrToFloat(AInputValue.Value) - StrToFloat(Result.Value));
    end;
  end;
  if Expr <> nil then
  begin
    Result := Expr.eval(Result);
  end;

end;


function TExpr.DetermineType: TValueType;
begin
  Result := vtUNDEFINED;
  if Term <> nil then Result := Term.EvalData.ValueType;
  if (Expr <> nil) and (Expr.EvalData.ValueType <> Term.EvalData.ValueType) then
    Result := vtUNDEFINED;
end;

{ TTerm }

constructor TTerm.Create(AFactor: TSyntaxNode; ATerm: TSyntaxNode; AOpToken: TToken = nil);
begin
  Factor := AFactor;
  Term := ATerm;
  EvalData := TExprValue.Create(DetermineType);

  if AOpToken <> nil then
  begin
    if (AOpToken.Tag = ttMULTIPLY) or (AOPToken.Tag = ttDIVIDE) then
    begin
      if (EvalData.ValueType <> vtNUM) then
        raise Exception.Create('Non NUM type operand used in Term');
      TokenTag := AOpToken.Tag;
      TokenLexeme := AOpToken.Lexeme;
      DisplayText := TokenLexeme;
    end
    else
      raise  Exception.Create(AOpToken.Lexeme + ' token passed to Term Syntax Node');
  end
  else
  begin
    TokenTag := ttNONE;
    DisplayText := 'Term';
  end;
  ChildrenList := TList.Create;
  if EvalData.ValueType = vtUNDEFINED then
    raise Exception.Create('Type mismatch in TTerm');
  ChildrenList.Add(Factor);
  ChildrenList.Add(Term);
  FreeAndNil(AOpToken);
end;

procedure TTerm.Gen;
begin
  if Factor <> nil then Factor.Gen;

  if TokenTag <> ttNONE then Write(' ' + TokenLexeme + ' ');
  //for correct postfix order 1/2*3

  if Term <> nil then Term.Gen;
end;

function TTerm.Eval(AInputValue: TExprValue = nil): TExprValue;
begin
  Result := Factor.eval();
  case TokenTag of
    ttMULTIPLY: begin
      Result.Value := FloatToStr(StrToFloat(AInputValue.Value) * StrToFloat(Result.Value));
    end;
    ttDIVIDE: begin
      Result.Value := FloatToStr(StrToFloat(AInputValue.Value) / StrToFloat(Result.Value));
    end;
  end;
  if Term <> nil then
  begin
    Result := Term.eval(Result);
  end;
end;



function TTerm.DetermineType: TValueType;
begin
  Result := vtUNDEFINED;
  if Factor <> nil then Result := Factor.EvalData.ValueType;
  if (Term <> nil) and (Term.EvalData.ValueType <> Factor.EvalData.ValueType) then
    Result := vtUNDEFINED;
end;


{ TSymbolTable }

constructor TSymbolTable.Create(Parent: TSymbolTable);
begin
  ParentTable := Parent;
  Symbols := TFPObjectHashTable.Create();
end;

destructor TSymbolTable.Destroy;
begin
  FreeAndNil(Symbols);
  inherited Destroy;
end;

procedure TSymbolTable.Add(ALexeme: string; ASymbol: TSymbol);
begin
  Symbols.Add(ALexeme, ASymbol);
end;

function TSymbolTable.GetType(ALexeme: string): TValueType;
begin
  if Symbols.Items[ALexeme] <> nil then
    Result := TSymbol(Symbols.Items[ALexeme]).EvalData.ValueType
  else
    Result := vtUNDEFINED;
end;

procedure TableIterator(Item: TObject; const key: string; var Continue: boolean);
var
  sType: string;
begin
  WriteStr(sType, TSymbol(Item).EvalData.ValueType);
  continue := True;
  Write(key, ' ', sType + ':' +
    TSymbol(Item).EvalData.Value + LineEnding);
end;

procedure TSymbolTable.PrintSymbols;

begin
  Symbols.Iterate(@TableIterator);
end;

procedure TSyntaxNode.SetDisplayText(AValue: string);
begin
  FDisplayText := AValue;
end;

procedure TSyntaxNode.Gen;
begin
  { each node has to implement its Gen method }
end;

function TSyntaxNode.Eval(AInputValue: TExprValue): TExprValue;
begin
  raise Exception.Create('TSyntaxNode Eval function should be implemented in subclasses');
end;


destructor TSyntaxNode.Destroy;
begin
  inherited Destroy;
end;

function TSyntaxNode.GetDisplayText: string;
begin
  Result := FDisplayText;
end;

function TSyntaxNode.PositionString: string;
var
  Row, Col: string;
begin
  Row := IntToStr(Lex.Lookahead.Position[0]);
  Col := IntToStr(Lex.Lookahead.Position[1]);
  Result := '@(' + Row + ',' + Col + ') ';
end;

{ TNodes }

constructor TNodes.Create;
begin
  ParseNode := TParseNode.Create;
  SyntaxNode := TSyntaxNode.Create;
end;

constructor TNodes.CreateAssign(var ParseVar: TParseNode; var SyntaxVar: TSyntaxNode);
begin
  ParseNode := TParseNode.Create();
  SyntaxNode := TSyntaxNode.Create();
  ParseVar := ParseNode;
  SyntaxVar := SyntaxNode;
end;

constructor TNodes.CreateAssignParse(var ParseVar: TParseNode);
begin
  ParseNode := TParseNode.Create();
  ParseVar := ParseNode;
end;

destructor TNodes.Destroy;
begin
  inherited Destroy;
end;


procedure TParser.FreeParseTree(Node: TParseNode);
var
  i: integer;
  ChildrenCount: integer = 0;
begin
  if Node <> nil then
  begin

    ChildrenCount := Node.ChildrenList.Count;
    for i := 0 to ChildrenCount - 1 do
    begin
      FreeParseTree(TParseNode(Node.ChildrenList.Items[i]));
    end;
    FreeAndNil(Node);

  end;

end;

function TParser.FindSymbol(AIdentifier: string): TSymbol;
var
  Table: TSymbolTable;
begin
  Table := SymbolTableCurrent;
  repeat
    Result := TSymbol(Table.Symbols.Items[AIdentifier]);
    if Result <> nil then
      Exit();
    Table := Table.ParentTable;
  until Table = nil;
end;

destructor TParser.Destroy;
begin
  FreeParseTree(ParseRoot);
  inherited Destroy;
end;

procedure TParser.CreateNewNode;
begin
  NewNode := TParseNode.Create();
end;

procedure TParser.CreateNewNodeAndLink(Node: TParseNode);
begin
  CreateNewNode;
  Node.Link(NewNode);
end;

procedure TParser.parse(FileNameToParse: string);
var
  Nodes: TNodes = nil;

begin
  FreeParseTree(ParseRoot);
  Lex := TLexer.Create(FileNameToParse);
  Lex.ReadLine();

  { #done : how to re-initialize ParseRoot and New Node in case of Parse re-run ? }
  try
    Lex.Advance();
    Nodes := prg();
    ParseRoot := Nodes.ParseNode;
    SyntaxRoot := Nodes.SyntaxNode;
    FreeAndNil(Nodes);
  except
    on E: Exception do
    begin
      { #todo : how to print partial parse tree when error ? }
      { #done : how to free memory in case of exception ? }
      writeln();
      writeln(E.message);
      if Nodes <> nil then
        PrintParseTree(ParseRoot);

      FreeAndNil(Lex);
      raise; // re-raise exception

      Exit;

    end;
  end;
  //PrintParseTree(ParseRoot, 0);
  //SyntaxRoot.Gen;

  WriteLn();
  WriteLn('Parsing finished with OK result' + LineEnding);
  WriteLn('WORDS TABLE CONTENT:');
  Lex.WordsTable.Iterate(@Lex.WordsIterator);
  WriteLn();
  FreeAndNil(Lex);

end;

procedure TParser.parseManual(AMemoLines: TStrings);
var
  Nodes: TNodes = nil;

begin
  FreeParseTree(ParseRoot);
  Lex := TLexer.CreateManual(AMemoLines);
  Lex.ReadLine();

  { #done : how to re-initialize ParseRoot and New Node in case of Parse re-run ? }
  try
    Lex.Advance();
    Nodes := prg();
    ParseRoot := Nodes.ParseNode;
    SyntaxRoot := Nodes.SyntaxNode;
    FreeAndNil(Nodes);
  except
    on E: Exception do
    begin
      { #todo : how to print partial parse tree when error ? }
      { #done : how to free memory in case of exception ? }
      writeln();
      writeln(E.message);
      if Nodes <> nil then
        PrintParseTree(ParseRoot);

      FreeAndNil(Lex);
      raise;

      Exit;

    end;
  end;
  //PrintParseTree(ParseRoot, 0);
  //SyntaxRoot.Gen;

  WriteLn();
  WriteLn('Parsing finished with OK result' + LineEnding);
  WriteLn('WORDS TABLE CONTENT:');
  Lex.WordsTable.Iterate(@Lex.WordsIterator);
  WriteLn();
  FreeAndNil(Lex);

end;


function TParser.prg(): TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.Name := 'Program';
  Result.SyntaxNode := TPrgSeq.Create;

  while Lex.Lookahead.Tag <> ttNONE do
  begin
    Nodes := block();
    ParseNode.Link(Nodes.ParseNode);
    TPrgSeq(Result.SyntaxNode).Link(Nodes.SyntaxNode);
    FreeAndNil(Nodes);
    //writeln('Return from block to prg');
  end;
end;

function TParser.block: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  StoredParent: TSymbolTable;

begin

  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.Name := 'block';

  case Lex.Lookahead.Tag of
    ttCURLY_LEFT: begin
      Lex.Match(ttCURLY_LEFT);
      // symbol table init
      StoredParent := SymbolTableCurrent;
      SymbolTableCurrent := TSymbolTable.Create(StoredParent);

      ParseNode.AddChildWithText('{');

      //writeln('Block start');
      Nodes := decls();

      ParseNode.Link(Nodes.ParseNode);
      FreeAndNil(Nodes);

      Nodes := statements();
      ParseNode.Link(Nodes.ParseNode);
      Result.SyntaxNode := Nodes.SyntaxNode;
      FreeAndNil(Nodes);
      Lex.Match(ttCURLY_RIGHT);
      ParseNode.AddChildWithText('}');
      writeln('Identifier values on block exit:');
      SymbolTableCurrent.PrintSymbols; // show current types and values b4 block exit
      SymbolTableCurrent := StoredParent;
    end;
    else
    begin
      { #done : correct char position report in case of syntax error }
      raise Exception.Create('Syntax error in : ' +
        IntToStr(Lex.CurrentLineNumber) + ',' + IntToStr(Lex.CharPosition) +
        LineEnding + 'Curly Brackets expected' + LineEnding + Lex.CurrentLine);
    end;
  end;

end;

function TParser.decls: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.Name := 'decls';

  while Lex.Lookahead.Tag = ttTYPENAME do
  begin
    Nodes := decl();
    ParseNode.Link(Nodes.ParseNode);
    FreeAndNil(Nodes);
  end;
end;

function TParser.decl: TNodes;
var
  ParseNode: TParseNode = nil;
  HelperString: string;
  Symbol: TSymbol;
  IdValue: TExprValue;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.Name := 'decl';

  HelperString := Lex.Lookahead.Lexeme;

  Symbol := TSymbol.Create;

  case Lex.Lookahead.Lexeme of
    'num': begin
      IdValue := TExprValue.Create(vtNUM, '');
    end;
    'bool': begin
      IdValue := TExprValue.Create(vtBOOL, '');
    end;
    else
    begin
      raise Exception.Create(PositionString + 'Unknown type in declaraion');
    end;

  end;

  Lex.Match(ttTYPENAME);

  Symbol.EvalData := IdValue;

  if Lex.Lookahead.Tag <> ttIDENTIFIER then
    raise  Exception.Create(PositionString + 'Identifier missing in declaration!');

  HelperString := HelperString + ' ' + Lex.Lookahead.Lexeme;

  Symbol.Position := Lex.Lookahead.Position;
  try
    SymbolTableCurrent.Add(Lex.Lookahead.Lexeme, Symbol);
  except
    on E: EDuplicate do writeln(LineEnding + E.message);
  end;

  Lex.Match(ttIDENTIFIER);
  HelperString := HelperString + ' ' + Lex.Lookahead.Lexeme;

  Lex.Match(ttSEMICOLON);
  ParseNode.AddChildWithText(HelperString);

end;

function TParser.assignment: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  Symbol: TSymbol;
  LeftSide: string;
  vt: TValueType;
begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.Name := 'assignment';
  ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
  Symbol := FindSymbol(Lex.Lookahead.Lexeme);
  if Symbol = nil then
  begin
    raise Exception.Create(PositionString + 'Identifier: ' + Lex.Lookahead.Lexeme +
      ' not declared!');
  end;
  LeftSide := Lex.Lookahead.Lexeme;
  Lex.Match(ttIDENTIFIER);
  ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
  Lex.Match(ttEQUAL_SIGN);
  {#todo: tu je asi chyba pri assignmente musim is zase od bool asi}
  //Nodes := expr();
  Nodes := bool();
  {#todo treba overovat typ uz tu alebo staci pri eval ? }
  Result.SyntaxNode := TAssign.Create(LeftSide, TExpr(Nodes.SyntaxNode));

  ParseNode.Link(Nodes.ParseNode);

  //vt := Nodes.SyntaxNode.Eval().ValueType;
  if Symbol.EvalData.ValueType = Nodes.SyntaxNode.Eval().ValueType then
  begin
    Symbol.EvalData := Nodes.SyntaxNode.Eval();
    Symbol.IsAssigned := True;
    FreeAndNil(Nodes);
  end
  else
    raise Exception.Create(PositionString + 'Right side  type mismatch with left side type');
end;

function TParser.statements(): TNodes;
  { #todo : add distinction statement vs expression }
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.Name := 'statements';
  Result.SyntaxNode := TSeq.Create;

  while (Lex.Lookahead.Tag <> ttCURLY_RIGHT) do
  begin
    case Lex.Lookahead.Tag of
      ttCURLY_LEFT: begin
        Nodes := block();
        ParseNode.Link(Nodes.ParseNode);
        TSeq(Result.SyntaxNode).Link(Nodes.SyntaxNode);
        FreeAndNil(Nodes);
      end;

      else
        if (Lex.Lookahead.Tag = ttIDENTIFIER) and (Lex.PeekCharNonWhite() = '=') then
        begin
          Nodes := assignment();
          ParseNode.Link(Nodes.ParseNode);
          TSeq(Result.SyntaxNode).Link(Nodes.SyntaxNode);
          FreeAndNil(Nodes);
          Lex.Match(ttSEMICOLON);


          ParseNode.AddChildWithText(';');
        end
        else
        begin
          //Nodes := expr();
          Nodes := bool();
          ParseNode.Link(Nodes.ParseNode);
          TSeq(Result.SyntaxNode).Link(Nodes.SyntaxNode);
          FreeAndNil(Nodes);

          Lex.Match(ttSEMICOLON);
          TSeq(Result.SyntaxNode).Link(TSemicolon.Create);
          // workaround to print ';' end of expression

          ParseNode.AddChildWithText(';');
        end;
    end;
    //writeln('Statements processed');

  end;

end;

function TParser.bool: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  ABool: TSyntaxNode;
  ABoolTerm: TSyntaxNode;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.Name := 'bool';

  Nodes := bool_term();
  ParseNode.Link(Nodes.ParseNode);
  ABoolTerm := Nodes.SyntaxNode;
  FreeAndNil(Nodes);

  Nodes := bool_rest();
  ParseNode.Link(Nodes.ParseNode);
  ABool := Nodes.SyntaxNode;
  FreeAndNil(Nodes);

  Result.SyntaxNode := TBool.Create(ABoolTerm, ABool);
end;

function TParser.bool_term: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.Name := 'bool_term';
 {
  case Lex.Lookahead.Tag of
    ttLEFT_PARENS: begin
      Lex.Match(ttLEFT_PARENS);
      Nodes := bool();
      ParseNode.Link(Nodes.ParseNode);
      Result.SyntaxNode := Nodes.SyntaxNode;
      FreeAndNil(Nodes);
      Lex.Match(ttRIGHT_PARENS);
    end;
    else
    begin
      Nodes := expr();
      ParseNode.Link(Nodes.ParseNode);
      Result.SyntaxNode := Nodes.SyntaxNode;
      FreeAndNil(Nodes);
    end;
  end;
 }
  Nodes := expr();
  ParseNode.Link(Nodes.ParseNode);
  Result.SyntaxNode := Nodes.SyntaxNode;
  FreeAndNil(Nodes);
end;

function TParser.bool_rest: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  ABoolTerm: TSyntaxNode;
  ABool: TSyntaxNode;
  AOpToken: TToken;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.Name := 'bool_rest';

  case Lex.Lookahead.Tag of
    ttOR: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      AOpToken := TToken.Create(Lex.Lookahead.Tag, Lex.Lookahead.Lexeme);
      Lex.Match(ttOR);

      Nodes := bool_term();
      ParseNode.Link(Nodes.ParseNode);
      ABoolTerm := Nodes.SyntaxNode;
      FreeAndNil(Nodes);
      Nodes := bool_rest();
      ParseNode.Link(Nodes.ParseNode);
      ABool := Nodes.SyntaxNode;
      FreeAndNil(Nodes);

      Result.SyntaxNode := TBool.Create(ABoolTerm, ABool, AOpToken);

    end;
    ttAND: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      AOpToken := TToken.Create(Lex.Lookahead.Tag, Lex.Lookahead.Lexeme);

      Lex.Match(ttAND);
      Nodes := bool();
      ParseNode.Link(Nodes.ParseNode);
      ABoolTerm := Nodes.SyntaxNode;
      FreeAndNil(Nodes);
      Nodes := bool_rest();
      ParseNode.Link(Nodes.ParseNode);
      ABool := Nodes.SyntaxNode;
      FreeAndNil(Nodes);

      Result.SyntaxNode := TBool.Create(ABoolTerm, ABool, AOpToken);

    end;

  end;
end;

function TParser.expr: TNodes;

var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  AExpr: TSyntaxNode;
  ATerm: TSyntaxNode;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.Name := 'expr';

  Nodes := term();
  ParseNode.Link(Nodes.ParseNode);
  ATerm := Nodes.SyntaxNode;
  FreeAndNil(Nodes);

  Nodes := expr_rest();
  ParseNode.Link(Nodes.ParseNode);
  AExpr := Nodes.SyntaxNode;
  FreeAndNil(Nodes);

  Result.SyntaxNode := TExpr.Create(ATerm, AExpr);
end;

function TParser.term: TNodes;

var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  AFactor: TSyntaxNode;
  ATerm: TSyntaxNode;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.Name := 'term';

  Nodes := factor();
  ParseNode.Link(Nodes.ParseNode);
  AFactor := Nodes.SyntaxNode;
  FreeAndNil(Nodes);

  Nodes := term_rest();
  ParseNode.Link(Nodes.ParseNode);
  ATerm := Nodes.SyntaxNode;
  FreeAndNil(Nodes);

  Result.SyntaxNode := TTerm.Create(AFactor, ATerm);
end;

function TParser.expr_rest: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  ATerm: TSyntaxNode;
  AExpr: TSyntaxNode;
  AToken: TToken;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.Name := 'expr_rest';

  case Lex.Lookahead.Tag of
    ttPLUS: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      AToken := TToken.Create(Lex.Lookahead.Tag, Lex.Lookahead.Lexeme);
      Lex.Match(ttPLUS);

      Nodes := term();
      ParseNode.Link(Nodes.ParseNode);
      ATerm := Nodes.SyntaxNode;
      FreeAndNil(Nodes);
      Nodes := expr_rest();
      ParseNode.Link(Nodes.ParseNode);
      AExpr := Nodes.SyntaxNode;
      FreeAndNil(Nodes);

      Result.SyntaxNode := TExpr.Create(ATerm, AExpr, AToken);

    end;
    ttMINUS: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      AToken := TToken.Create(Lex.Lookahead.Tag, Lex.Lookahead.Lexeme);

      Lex.Match(ttMINUS);
      Nodes := term();
      ParseNode.Link(Nodes.ParseNode);
      ATerm := Nodes.SyntaxNode;
      FreeAndNil(Nodes);
      Nodes := expr_rest();
      ParseNode.Link(Nodes.ParseNode);
      AExpr := Nodes.SyntaxNode;
      FreeAndNil(Nodes);

      Result.SyntaxNode := TExpr.Create(ATerm, AExpr, AToken);

    end;

  end;
end;

function TParser.term_rest: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  AFactor: TSyntaxNode;
  ATerm: TSyntaxNode;
  AToken: TToken;
begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.Name := 'term_rest';
  case Lex.Lookahead.Tag of
    ttMULTIPLY: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      AToken := TToken.Create(Lex.Lookahead.Tag, Lex.Lookahead.Lexeme);
      Lex.Match(ttMULTIPLY);

      Nodes := factor();
      ParseNode.Link(Nodes.ParseNode);
      AFactor := Nodes.SyntaxNode;

      FreeAndNil(Nodes);
      Nodes := term_rest();
      ParseNode.Link(Nodes.ParseNode);
      ATerm := Nodes.SyntaxNode;
      FreeAndNil(Nodes);

      Result.SyntaxNode := TTerm.Create(AFactor, ATerm, AToken);

    end;
    ttDIVIDE: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      AToken := TToken.Create(Lex.Lookahead.Tag, Lex.Lookahead.Lexeme);

      Lex.Match(ttDIVIDE);
      Nodes := factor();

      ParseNode.Link(Nodes.ParseNode);
      AFactor := Nodes.SyntaxNode;

      FreeAndNil(Nodes);
      Nodes := term_rest();
      ParseNode.Link(Nodes.ParseNode);
      ATerm := Nodes.SyntaxNode;
      FreeAndNil(Nodes);

      Result.SyntaxNode := TTerm.Create(AFactor, ATerm, AToken);

    end;

  end;
end;

procedure TParser.PrintParseTree(Node: TParseNode; SpaceCount: word = 0);
var
  ChildrenCount: integer = 0;
  i: integer = 0;
begin
  if Node <> nil then
  begin
    // print spaces first
    ChildrenCount := Node.ChildrenList.Count;
    for i := 1 to SpaceCount do
    begin
      Write(' ');
    end;
    writeln(Node.Name);
    for i := 0 to (ChildrenCount - 1) do
    begin
      PrintParseTree(TParseNode(Node.ChildrenList.Items[i]), SpaceCount + 1);
    end;
  end;
end;

procedure TParser.ShowParseTree(AParseNode: TParseNode; ATreeView: TTreeView);

  procedure _AddViewNode(AParseNode: TParseNode; AViewNode: TTreeNode);
  var
    NewViewNode: TTreeNode;
    i: integer = 0;
    ChildrenCount: integer = 0;
  begin
    ChildrenCount := AParseNode.ChildrenList.Count;
    NewViewNode := ATreeView.Items.AddChild(AViewNode, AParseNode.Name);
    for i := 0 to (ChildrenCount - 1) do
    begin
      _AddViewNode(TParseNode(AParseNode.ChildrenList.Items[i]), NewViewNode);
    end;
  end;

begin
  ATreeView.Items.Clear;
  _AddViewNode(AParseNode, nil);
  ATreeView.FullExpand;
end;

procedure TParser.PrintSyntaxTree(ANode: TSyntaxNode; SpaceCount: word);
var
  ChildrenCount: integer = 0;
  i: integer = 0;
begin
  if ANode <> nil then
  begin
    // print spaces first
    if ANode.ChildrenList <> nil then
      ChildrenCount := ANode.ChildrenList.Count
    else
      ChildrenCount := 0;
    for i := 1 to SpaceCount do
    begin
      Write(' ');
    end;
    writeln(ANode.DisplayText);
    for i := 0 to (ChildrenCount - 1) do
    begin
      PrintSyntaxTree(TSyntaxNode(ANode.ChildrenList.Items[i]), SpaceCount + 1);
    end;
  end;
end;

function TParser.PositionString: string;
var
  Row, Col: string;
begin
  Row := IntToStr(Lex.Lookahead.Position[0]);
  Col := IntToStr(Lex.Lookahead.Position[1]);
  Result := '@(' + Row + ',' + Col + ') ';
end;

function TParser.factor: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  _Symbol: TSymbol;
  _Token: TToken;
  row: integer;
  col: integer;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.Name := 'factor';

  case Lex.Lookahead.Tag of
    ttMINUS: begin
      Lex.Match(ttMINUS); // processing negative ttNUMBER sign
      ParseNode.AddChildWithText('-' + Lex.Lookahead.Lexeme);
      Result.SyntaxNode := TFactor.Create(ttNUMBER, '-' + Lex.Lookahead.Lexeme);
      Result.SyntaxNode.EvalData := TExprValue.Create(vtNUM, '-' + Lex.Lookahead.Lexeme);
      Lex.Match(ttNUMBER);
    end;

    ttNUMBER: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Result.SyntaxNode := TFactor.Create(ttNUMBER, Lex.Lookahead.Lexeme);
      Result.SyntaxNode.EvalData := TExprValue.Create(vtNUM, Lex.Lookahead.Lexeme);
      Lex.Match(ttNUMBER);
    end;

    ttTRUE: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Result.SyntaxNode := TFactor.Create(ttTRUE, Lex.Lookahead.Lexeme);
      Result.SyntaxNode.EvalData := TExprValue.Create(vtBOOL, Lex.Lookahead.Lexeme);
      Lex.Match(ttTRUE);
    end;

    ttFALSE: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Result.SyntaxNode := TFactor.Create(ttFALSE, Lex.Lookahead.Lexeme);
      Result.SyntaxNode.EvalData := TExprValue.Create(vtBOOL, Lex.Lookahead.Lexeme);
      Lex.Match(ttFALSE);
    end;

    ttIDENTIFIER: begin
      { #done : add searching whole symbol table chain }
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      row := Lex.Lookahead.Position[0];
      col := Lex.Lookahead.Position[1];
      _Symbol := FindSymbol(Lex.Lookahead.Lexeme);

      if _Symbol = nil then
      begin
        raise Exception.Create('(' + IntToStr(row) + ',' + IntToStr(col) +
          ')' + ' Identifier ' + Lex.Lookahead.Lexeme + ' is not declared');
      end;

      if _Symbol.IsAssigned = False then
        raise Exception.Create('@(' + IntToStr(row) + ',' + IntToStr(col) +
          ') ' + '"' + Lex.Lookahead.Lexeme + '"' +
          ' is used in expression without being assigned a value!');

      Result.SyntaxNode := TFactor.Create(Lex.Lookahead.Tag, _Symbol.EvalData.Value);
      Result.SyntaxNode.EvalData := _Symbol.EvalData;
      Lex.Match(ttIDENTIFIER);
    end;
    ttLEFT_PARENS: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Lex.Match(ttLEFT_PARENS);
      Nodes := bool();
      ParseNode.Link(Nodes.ParseNode);
      Result.SyntaxNode := Nodes.SyntaxNode;
      FreeAndNil(Nodes);
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Lex.Match(ttRIGHT_PARENS);
    end;
    else
    begin
      raise Exception.Create('@(' + IntToStr(Lex.CurrentLineNumber) +
        ',' + IntToStr(Lex.CharPosition) + ')' +
        ' Num, Bool, Identifier or Left Parens expected' +
        LineEnding + Lex.CurrentLine);
    end;
  end;
end;

{ TParseNode }

constructor TParseNode.Create;
begin
  ChildrenList := TList.Create;
end;

destructor TParseNode.Destroy;
begin
  FreeAndNil(ChildrenList);
  inherited Destroy;
end;


procedure TParseNode.Link(Node: TParseNode);
begin
  ChildrenList.add(Node);
end;

procedure TParseNode.AddChildWithText(TextToAdd: string);
var
  Node: TParseNode;
begin
  Node := TParseNode.Create();
  Node.Name := TextToAdd;
  Link(Node);
end;


end.
