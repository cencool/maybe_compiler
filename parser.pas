(*
Grammar:                               Syntax nodes:
prg -> block prg                       prg.n = PrgSeq(Nodes: TSyntaxNode...)
      | block                          prg.n = PrgSeq(Nodes: TSyntaxNode...)
block ->   { decls statements }        block.n = statements.n
decls -> decl decls                    decls.n = nil
        | @                            decls.n = nil
decl -> TYPENAME ID ;                  decl.n = nil

statements -> block                    statements.n = block.n
            | assignment ; statements  statements.n = Seq(Nodes: TSyntaxNode...)
            | expr ; statements        statements.n = Seq(Nodes: TSyntaxNode...)
            | @                        statements.n = Seq(Nodes: TSyntaxNode...)

expr -> term rest                      expr.n =  Expr(Term();Expr(); nil)
expr_rest -> + term rest               expr_rest.n = Expr(Term();Expr(); Lookahead)
            | - term rest | @          expr_rest.n =  Expr(Term();Expr(); Lookahead)

term -> factor term_rest               term.n =  Term(Factor(); Term(); nil)
term_rest -> * factor term_rest        term_rest.n =  Term(Factor(); Term(); Lookahead)
           | / factor term_rest        term_rest.n =  Term(Factor(); Term(); Lookahead)
           | @                         term_rest.n = nil

factor -> ID                           factor.n = Factor (Lookahead)
        | NUMBER                       factor.n =  Factor (Lookahead)
        | ( expr)                      factor.n = expr.n

assignment -> ID = expr                assignment.n =  Assign(LeftSide, RightSide)

@ means empty production

*)
{ #todo : finish grammar comment }
{ #done : zda sa ze prepis do postscript notacie pre napr. 1/2*3 nie je dobre }
unit Parser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  Lexer,
  Contnrs,
  ComCtrls;

type

  { TParseNode }

  TParseNode = class
    Name: string;
    DisplayText: string;
    ChildrenList: TList;
    constructor Create();
    destructor Destroy; override;
    procedure Link(Node: TParseNode);
    procedure AddChildWithText(TextToAdd: string);
  end;

  { TSyntaxNode }

  TSyntaxNode = class
    FDisplayText: string;
    ChildrenList: TList;
    procedure Gen; virtual;
    function Eval(AInputValue: extended = 0): extended; virtual;
    destructor Destroy; override;
    function GetDisplayText: string; virtual;
    property DisplayText: string read GetDisplayText;
  end;

  { TExpr }

  TExpr = class(TSyntaxNode)
    FExpr: TSyntaxNode;
    FTerm: TSyntaxNode;
    FTokenTag: TTokenTag;
    FTokenLexeme: string;
    constructor Create(ATerm: TSyntaxNode; AExpr: TSyntaxNode; AOpToken: TToken = nil);
    procedure Gen; override;
    function Eval(AInputValue: extended = 0): extended; override;
  end;

  { TTermNode }

  TTerm = class(TSyntaxNode)
    FFactor: TSyntaxNode;
    FTerm: TSyntaxNode;
    FTokenTag: TTokenTag;
    FTokenLexeme: string;
    constructor Create(AFactor: TSyntaxNode; ATerm: TSyntaxNode; AOpToken: TToken = nil);
    procedure Gen; override;
    function Eval(AInputValue: extended = 0): extended; override;
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
    FTokenTag: TTokenTag;
    FTokenLexeme: string;
    FFactorValue: extended;
    constructor Create(AToken: TToken; AValue: extended = 0);
    procedure Gen; override;
    function Eval(AInputValue: extended = 0): extended; override;
  end;

  { TSemicolon }

  TSemicolon = class(TSyntaxNode)
    procedure Gen; override;
    constructor Create;
  end;

  { TAssign }

  TAssign = class(TSyntaxNode)
    { #todo co vlastne chcem aby robil ? }
    FLeftSide: string;
    FRightSide: TExpr;
    constructor Create(ALeftSide: string; ARightSide: TExpr = nil);
    procedure Gen; override;

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

  TSymbol = class
    SymbolType: string;
    SymbolValue: extended;
    isAssigned: boolean;
    SymbolPosition: array [0..1] of integer;
  end;

  { TSymbolTable }

  TSymbolTable = class
    ParentTable: TSymbolTable;
    Symbols: TFPObjectHashTable;
    constructor Create(Parent: TSymbolTable);
    destructor Destroy; override;
    procedure Add(ALexeme: string; ASymbol: TSymbol);
    function GetType(ALexeme: string): string;
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
    function expr(): TNodes;
    function term(): TNodes;
    function factor(): TNodes;
    function expr_rest(): TNodes;
    function term_rest(): TNodes;
    procedure PrintParseTree(Node: TParseNode; SpaceCount: word = 0);
    procedure ShowParseTree(AParseNode: TParseNode; ATreeView: TTreeView);
    procedure PrintSyntaxTree(ANode: TSyntaxNode; SpaceCount: word = 0);
    procedure FreeParseTree(Node: TParseNode);
    function FindSymbol(AIdentifier: string): TSymbol;
  end;


implementation

uses
  SysUtils;

var
  Lex: TLexer;

{ TAssign }

constructor TAssign.Create(ALeftSide: string; ARightSide: TExpr = nil);
begin
  FLeftSide := ALeftSide;
  FRightSide := ARightSide;
  FDisplayText := 'Assign';
  if ARightSide <> nil then
  begin
    ChildrenList := TList.Create;
    ChildrenList.Add(ARightSide);
  end;
end;

procedure TAssign.Gen;
begin
  Write(' ' + FLeftSide + ' = ');
  FRightSide.Gen;
  Write(';' + LineEnding);

end;


{ TSemicolon }

procedure TSemicolon.Gen;
begin
  WriteLn(';');
end;

constructor TSemicolon.Create;
begin
  FDisplayText := ';';
end;

{ TPrgSeq }

constructor TPrgSeq.Create;
begin
  FPrgSeq := TList.Create;
  FDisplayText := 'PrgSeq';
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

constructor TFactor.Create(AToken: TToken; Avalue: extended = 0);
begin
  FTokenTag := AToken.Tag;
  FTokenLexeme := AToken.Lexeme;
  FFactorValue := Avalue;
  FDisplayText := FTokenLexeme;
end;

procedure TFactor.Gen;
begin
  Write(' ' + FTokenLexeme + ' ');
end;

function TFactor.Eval(AInputValue: extended): extended;
begin
  case FTokenTag of
    NUMBER: begin
      Result := StrToFloat(FTokenLexeme);
    end;
    IDENTIFIER: begin
      Result := FFactorValue;
    end;
  end;
end;

{ TSequence }

constructor TSeq.Create;
begin
  FExprSeq := TList.Create;
  FDisplayText := 'Seq';
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
  if AOpToken <> nil then
  begin
    if (AOpToken.Tag = PLUS) or (AOPToken.Tag = MINUS) then
    begin
      FTokenTag := AOpToken.Tag;
      FTokenLexeme := AOpToken.Lexeme;
      FDisplayText := FTokenLexeme;
    end
    else
      raise  Exception.Create(AOpToken.Lexeme + ' token passed to Expr Syntax Node');
  end
  else
  begin
    FTokenTag := NONE;
    FDisplayText := 'Expr';
  end;
  ChildrenList := TList.Create;
  FTerm := ATerm;
  FExpr := AExpr;
  ChildrenList.Add(FTerm);
  ChildrenList.Add(FExpr);
  FreeAndNil(AOpToken);
end;

procedure TExpr.Gen;
begin
  if FTerm <> nil then FTerm.Gen;
  if FTokenTag <> NONE then Write(' ' + FTokenLexeme + ' '); // for correct postfix order
  if FExpr <> nil then FExpr.Gen;

end;

function TExpr.Eval(AInputValue: extended = 0): extended;
begin
  { building result of expression based on available components of exp }
  Result := FTerm.Eval();
  case FTokenTag of
    PLUS: begin
      Result := AInputValue + Result;
    end;
    MINUS: begin
      Result := AInputValue - Result;
    end;
  end;
  if FExpr <> nil then
  begin
    Result := FExpr.eval(Result);
  end;

end;

{ TTerm }

constructor TTerm.Create(AFactor: TSyntaxNode; ATerm: TSyntaxNode;
  AOpToken: TToken = nil);
begin
  if AOpToken <> nil then
  begin
    if (AOpToken.Tag = MULTIPLY) or (AOPToken.Tag = DIVIDE) then
    begin
      FTokenTag := AOpToken.Tag;
      FTokenLexeme := AOpToken.Lexeme;
      FDisplayText := FTokenLexeme;
    end
    else
      raise  Exception.Create(AOpToken.Lexeme + ' token passed to Term Syntax Node');
  end
  else
  begin
    FTokenTag := NONE;
    FDisplayText := 'Term';
  end;
  ChildrenList := TList.Create;
  FFactor := AFactor;
  FTerm := ATerm;
  ChildrenList.Add(FFactor);
  ChildrenList.Add(FTerm);
  FreeAndNil(AOpToken);
end;

procedure TTerm.Gen;
begin
  if FFactor <> nil then FFactor.Gen;

  if FTokenTag <> NONE then Write(' ' + FTokenLexeme + ' ');
  //for correct postfix order 1/2*3

  if FTerm <> nil then FTerm.Gen;
end;

function TTerm.Eval(AInputValue: extended = 0): extended;
begin
  Result := FFactor.eval();
  case FTokenTag of
    MULTIPLY: begin
      Result := AInputValue * Result;
    end;
    DIVIDE: begin
      Result := AInputValue / Result;
    end;
  end;
  if FTerm <> nil then
  begin
    Result := FTerm.eval(Result);
  end;
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

function TSymbolTable.GetType(ALexeme: string): string;
begin
  if Symbols.Items[ALexeme] <> nil then
    Result := TSymbol(Symbols.Items[ALexeme]).SymbolType
  else
    Result := '';
end;

procedure TableIterator(Item: TObject; const key: string; var Continue: boolean);
begin
  continue := True;
  Write(key, ' ', TSymbol(Item).SymbolType + ':' +
    FloatToStr(TSymbol(Item).SymbolValue) + LineEnding);
end;

procedure TSymbolTable.PrintSymbols;

begin
  Symbols.Iterate(@TableIterator);
end;

procedure TSyntaxNode.Gen;
begin
  { each node has to implement its Gen method }
end;

function TSyntaxNode.Eval(AInputValue: extended = 0): extended;
begin
  Result := 0;
end;


destructor TSyntaxNode.Destroy;
begin
  inherited Destroy;
end;

function TSyntaxNode.GetDisplayText: string;
begin
  Result := FDisplayText;
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
      raise;

      Exit;

    end;
  end;
  //PrintParseTree(ParseRoot, 0);
  //SyntaxRoot.Gen;

  WriteLn();
  WriteLn('Parsing finished with OK result' + LineEnding);
  WriteLn('WORDS TABLE CONTENT:');
  Lex.Words.Iterate(@Lex.WordsIterator);
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
  Lex.Words.Iterate(@Lex.WordsIterator);
  WriteLn();
  FreeAndNil(Lex);

end;


function TParser.prg(): TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'Program';
  Result.SyntaxNode := TPrgSeq.Create;

  while Lex.Lookahead.Tag <> NONE do
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
  ParseNode.DisplayText := 'block';

  case Lex.Lookahead.Tag of
    CURLY_LEFT: begin
      Lex.Match(CURLY_LEFT);
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
      Lex.Match(CURLY_RIGHT);
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
  ParseNode.DisplayText := 'decls';

  while Lex.Lookahead.Tag = TYPENAME do
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

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'decl';

  HelperString := Lex.Lookahead.Lexeme;

  Symbol := TSymbol.Create;
  Symbol.SymbolType := Lex.Lookahead.Lexeme;

  Lex.Match(TYPENAME);

  if Lex.Lookahead.Tag <> IDENTIFIER then
    raise  Exception.Create('Identifier missing in declaration!');

  HelperString := HelperString + ' ' + Lex.Lookahead.Lexeme;

  Symbol.SymbolPosition := Lex.Lookahead.LexemePosition[0];
  try
    SymbolTableCurrent.Add(Lex.Lookahead.Lexeme, Symbol);
  except
    on E: EDuplicate do writeln(LineEnding + E.message);
  end;

  Lex.Match(IDENTIFIER);
  HelperString := HelperString + ' ' + Lex.Lookahead.Lexeme;

  Lex.Match(SEMICOLON);
  ParseNode.AddChildWithText(HelperString);

end;

function TParser.assignment: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  Symbol: TSymbol;
  LeftSide: string;
begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'assignment';
  ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
  Symbol := FindSymbol(Lex.Lookahead.Lexeme);
  if Symbol = nil then
  begin
    raise Exception.Create('Identifier: ' + Lex.Lookahead.Lexeme + ' not declared!');
  end;
  LeftSide := Lex.Lookahead.Lexeme;
  Lex.Match(IDENTIFIER);
  ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
  Lex.Match(EQUAL_SIGN);
  Nodes := expr();
  Result.SyntaxNode := TAssign.Create(LeftSide, TExpr(Nodes.SyntaxNode));

  //TAssign(Result.SyntaxNode).FRightSide := TExpr(Nodes.SyntaxNode);

  ParseNode.Link(Nodes.ParseNode);
  //ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
  Symbol.SymbolValue := Nodes.SyntaxNode.Eval();
  Symbol.isAssigned := True;
  FreeAndNil(Nodes);
  //Lex.Match(SEMICOLON);
end;

function TParser.statements(): TNodes;
  { #todo : add distinction statement vs expression }
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'statements';
  Result.SyntaxNode := TSeq.Create;

  while (Lex.Lookahead.Tag <> CURLY_RIGHT) do
  begin
    case Lex.Lookahead.Tag of
      CURLY_LEFT: begin
        Nodes := block();
        ParseNode.Link(Nodes.ParseNode);
        TSeq(Result.SyntaxNode).Link(Nodes.SyntaxNode);
        FreeAndNil(Nodes);
      end;

      else
        if (Lex.Lookahead.Tag = IDENTIFIER) and (Lex.PeekCharNonWhite() = '=') then
        begin
          Nodes := assignment();
          ParseNode.Link(Nodes.ParseNode);
          TSeq(Result.SyntaxNode).Link(Nodes.SyntaxNode);
          FreeAndNil(Nodes);
          Lex.Match(SEMICOLON);


          ParseNode.AddChildWithText(';');
        end
        else
        begin
          Nodes := expr();
          ParseNode.Link(Nodes.ParseNode);
          TSeq(Result.SyntaxNode).Link(Nodes.SyntaxNode);
          FreeAndNil(Nodes);

          Lex.Match(SEMICOLON);
          TSeq(Result.SyntaxNode).Link(TSemicolon.Create);
          // workaround to print ';' end of expression

          ParseNode.AddChildWithText(';');
        end;
    end;
    //writeln('Statements processed');

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
  ParseNode.DisplayText := 'expr';

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
  ParseNode.DisplayText := 'term';

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
  ParseNode.DisplayText := 'expr_rest';

  case Lex.Lookahead.Tag of
    PLUS: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      AToken := TToken.Create(Lex.Lookahead.Tag, Lex.Lookahead.Lexeme);
      Lex.Match(PLUS);

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
    MINUS: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      AToken := TToken.Create(Lex.Lookahead.Tag, Lex.Lookahead.Lexeme);

      Lex.Match(MINUS);
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
  ParseNode.DisplayText := 'term_rest';
  case Lex.Lookahead.Tag of
    MULTIPLY: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      AToken := TToken.Create(Lex.Lookahead.Tag, Lex.Lookahead.Lexeme);
      Lex.Match(MULTIPLY);

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
    DIVIDE: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      AToken := TToken.Create(Lex.Lookahead.Tag, Lex.Lookahead.Lexeme);

      Lex.Match(DIVIDE);
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
    writeln(Node.DisplayText);
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
    NewViewNode := ATreeView.Items.AddChild(AViewNode, AParseNode.DisplayText);
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

function TParser.factor: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  s: string;
  _Symbol: TSymbol;
  row: integer;
  col: integer;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'factor';

  case Lex.Lookahead.Tag of
    NUMBER: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Result.SyntaxNode := TFactor.Create(Lex.Lookahead);
      Lex.Match(NUMBER);
    end;
    IDENTIFIER: begin
      { #done : add searching whole symbol table chain }
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      row := Lex.Lookahead.LexemePosition[0][0];
      col := Lex.Lookahead.LexemePosition[0][1];
      _Symbol := FindSymbol(Lex.Lookahead.Lexeme);
      if _Symbol <> nil then
      begin
        s := _Symbol.SymbolType;
      end
      else
        raise Exception.Create('(' + IntToStr(row) + ',' + IntToStr(col) +
          ')' + ' Identifier ' + Lex.Lookahead.Lexeme + ' is not declared');
      if _Symbol.isAssigned <> True then
        raise Exception.Create('@(' + IntToStr(row) + ',' + IntToStr(col) +
          ') ' + '"' + Lex.Lookahead.Lexeme + '"' +
          ' is used in expression without being assigned a value!');
      Result.SyntaxNode := TFactor.Create(Lex.Lookahead, _Symbol.SymbolValue);
      Lex.Match(IDENTIFIER);
    end;
    LEFT_PARENS: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Lex.Match(LEFT_PARENS);
      Nodes := expr();
      ParseNode.Link(Nodes.ParseNode);
      Result.SyntaxNode := Nodes.SyntaxNode;
      FreeAndNil(Nodes);
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Lex.Match(RIGHT_PARENS);
    end
    else
    begin
      raise Exception.Create('@(' + IntToStr(Lex.CurrentLineNumber) +
        ',' + IntToStr(Lex.CharPosition) + ')' +
        ' Num or Identifier or Left parens expected' +
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
  Node.DisplayText := TextToAdd;
  Link(Node);
end;


end.
