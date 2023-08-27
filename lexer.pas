unit Lexer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Contnrs;

type
  TTokenTag = (
    ttNONE, ttNUMBER, ttIDENTIFIER, ttPLUS,
    ttMINUS, ttMULTIPLY, ttDIVIDE, ttLEFT_PARENS,
    ttRIGHT_PARENS, ttSEMICOLON, ttFILE_END, ttUNKNOWN,
    ttCURLY_LEFT, ttCURLY_RIGHT, ttTYPENAME, ttEQUAL_SIGN,
    ttAND, ttOR, ttIS_EQUAL, ttTRUE, ttFALSE
    );

  { TToken }

  TPosition = array [0..1] of integer;

  TToken = class
    Tag: TTokenTag;
    Lexeme: string;
    constructor Create(ATag: TTokenTag; ALexeme: string);
  end;

  { TLookahead }

  TLookahead = class
  private
    FToken: TToken;
    function GetLexeme: string;
    function GetTag: TTokenTag;
    procedure SetLexeme(AValue: string);
    procedure SetTag(AValue: TTokenTag);
    procedure SetToken(AValue: TToken);
  public
    Position: TPosition;
    constructor Create(ATag: TTokenTag; ALexeme: string; ARow: integer = 0; ACol: integer = 0);
    destructor Destroy; override;
    property Tag: TTokenTag read GetTag write SetTag;
    property Lexeme: string read GetLexeme write SetLexeme;
    property Token: TToken read FToken write SetToken;
  end;

  { TWord }

  TWord = class
    Token: TToken;
    Positions: array of TPosition;
    constructor Create(AToken: TToken);
    destructor Destroy; override;
  end;

  { TLexer }

  TLexer = class

  private
    FLookahead: TLookahead;
    procedure setLookahead(AValue: TLookahead);

  public
    SrcLines: TStringList;
    CurrentLine: string;
    PeekedLine: string;
    CurrentLineLength: word;
    CurrentLineNumber: word;
    PeekedLineLength: word;
    CharPosition: word;
    CurrentChar: string;
    PeekedChar: string;
    WordsTable: TFPObjectHashTable;

    constructor Create(const SrcFileName: string);
    constructor CreateManual(AMemoStrings: TStrings);
    destructor Destroy; override;

    procedure Advance();
    procedure AddWordPosition(APosition: TPosition; AWord: TWord);
    procedure IsNumber(negative: boolean);
    function InitWordsTable: TFPObjectHashTable;
    procedure Match(checked_token: TTokenTag);
    procedure PeekChar();
    function PeekCharNonWhite(): string;
    function PeekLine(src: TStringList): boolean;
    procedure ReadChar();
    function ReadLine(): boolean;
    procedure UpdateLookaheadToken(ATag: TTokenTag; ALexeme: string);
    procedure WordsIterator(Item: TObject; const key: string; var Continue: boolean);
    property Lookahead: TLookahead read FLookahead write setLookahead;


  end;



implementation

uses
  SysUtils, LazUTF8, Character;

{ TWord }

constructor TWord.Create(AToken: TToken);
begin
  Token := AToken;
end;

destructor TWord.Destroy;
begin
  FreeAndNil(Token);
  inherited Destroy;
end;

{ TLookahead }

procedure TLookahead.SetToken(AValue: TToken);
begin
  if FToken = AValue then Exit;
  FreeAndNil(FToken);
  FToken := AValue;
end;

function TLookahead.GetTag: TTokenTag;
begin
  Result := FToken.Tag;
end;

procedure TLookahead.SetLexeme(AValue: string);
begin
  FToken.Lexeme := AValue;
end;

function TLookahead.GetLexeme: string;
begin
  Result := FToken.Lexeme;
end;

procedure TLookahead.SetTag(AValue: TTokenTag);
begin
  FToken.Tag := AValue;
end;

constructor TLookahead.Create(ATag: TTokenTag; ALexeme: string; ARow: integer = 0;
  ACol: integer = 0);
begin
  FToken:= TToken.Create(ATag, ALexeme);
  Position[0] := ARow;
  Position[1] := ACol;
end;

destructor TLookahead.Destroy;
begin
  FreeAndNil(FToken);
  inherited Destroy;
end;

procedure TLexer.setLookahead(AValue: TLookahead);
begin
  if FLookahead = AValue then Exit;
  FreeAndNil(FLookahead);
  FLookahead := AValue;
end;

constructor TLexer.Create(const SrcFileName: string);
begin
  SrcLines := TStringList.Create;
  SrcLines.LoadFromFile(SrcFileName);
  Lookahead := TLookahead.Create(ttNONE, '');
  WordsTable := InitWordsTable;
end;

constructor TLexer.CreateManual(AMemoStrings: TStrings);
begin
  SrcLines := TStringList.Create;
  SrcLines.AddStrings(AMemoStrings);
  Lookahead := TLookahead.Create(ttNONE, '');
  WordsTable := InitWordsTable;

end;

destructor TLexer.Destroy;
begin
  FreeAndNil(SrcLines);
  FreeAndNil(FLookahead);
  FreeAndNil(WordsTable);
  inherited Destroy;
end;

function TLexer.ReadLine(): boolean;
begin
  if (CurrentLineNumber < SrcLines.Count) then
  begin
    CurrentLine := SrcLines.Strings[CurrentLineNumber];
    Inc(CurrentLineNumber);
    CurrentLineLength := UTF8length(CurrentLine);
    CharPosition := 0;
    Result := True;
  end
  else
  begin
    CurrentLineLength := 0;
    CurrentLine := '';
    CharPosition := 0;
    Result := False;
  end;

end;

procedure TLexer.UpdateLookaheadToken(ATag: TTokenTag; ALexeme: string);
begin
  Lookahead.Tag := ATag;
  Lookahead.Lexeme := ALexeme;
end;

function TLexer.PeekLine(src: TStringList): boolean;
begin
  if (CurrentLineNumber < src.Count) then
  begin
    PeekedLine := src.Strings[CurrentLineNumber];
    PeekedLineLength := UTF8Length(PeekedLine);
    Result := True;
  end
  else
  begin
    Result := False;
    PeekedLine := '';
    PeekedLineLength := 0;
  end;
end;

procedure TLexer.ReadChar();
begin
  Inc(CharPosition);
  if CharPosition <= CurrentLineLength then
  begin
    CurrentChar := utf8copy(CurrentLine, CharPosition, 1);
  end
  else
  if (CharPosition - CurrentLineLength) = 1 then CurrentChar := LineEnding
  else
  if ReadLine() then
  begin
    CharPosition := 0;
    ReadChar();
  end
  else
  begin
    CurrentChar := '';
    CharPosition := 0;
  end;
  PeekChar();  // peek char automatically with read char
end;

{ #done : isn't better to get peek character every time readchar is done ? }
procedure TLexer.PeekChar();
begin
  if CharPosition < CurrentLineLength then
    PeekedChar := UTF8copy(CurrentLine, CharPosition + 1, 1)
  else
  if CharPosition = CurrentLineLength then PeekedChar := LineEnding
  else
  if PeekLine(SrcLines) then
  begin
    PeekedChar := UTF8copy(PeekedLine, 1, 1);
    { #done: peekchar how about if next line is just newline ? }
  end
  else
  begin
    PeekedChar := '';
  end;

end;

{ returns first non-white character after current char or empty string if ttNONE }
function TLexer.PeekCharNonWhite: string;
var
  PeekPosition: integer;
  PeekLineLength: integer;
  PeekLineNumber: integer;
  _PeekLine: string;

begin
  Result := '';
  PeekPosition := CharPosition + 1;
  PeekLineLength := CurrentLineLength;
  PeekLineNumber := CurrentLineNumber;
  _PeekLine := CurrentLine;

  repeat

    if PeekPosition <= PeekLineLength then
    begin
      Result := UTF8copy(_PeekLine, PeekPosition, 1);
    end
    else
    if PeekPosition > PeekLineLength then
    begin
      PeekPosition := 1;
      if PeekLineNumber < SrcLines.Count then
      begin
        _PeekLine := SrcLines.Strings[PeekLineNumber];
        PeekLineLength := UTF8Length(_PeekLine);
        Inc(PeekLineNumber);
        if PeekLineLength = 0 then Result := LineEnding
        else
          Result := UTF8copy(_PeekLine, PeekPosition, 1);
      end
      else
      begin
        Result := '';
      end;
    end;
    Inc(PeekPosition);

  until (Result = '') or (not IsWhiteSpace(utf8decode(Result), 1));
end;

procedure TLexer.Advance();
var
  _Word: TWord = nil;
  s: string = '';
  TokenPos: array [0..1] of integer;
  _Token: TToken;

begin
  ReadChar();

  { skip white space }
  while
    (CurrentChar <> '') and (IsWhiteSpace(utf8decode(CurrentChar), 1) or
      (CurrentChar = LineEnding)) do ReadChar();

  TokenPos[0] := CurrentLineNumber;
  TokenPos[1] := CharPosition;
  case CurrentChar of
    '+': begin
      UpdateLookaheadToken(ttPLUS, '+');
      Lookahead.Position := TokenPos;
      Exit();
    end;
    '-': begin
      UpdateLookaheadToken(ttMINUS, '-');
      Lookahead.Position := TokenPos;
      Exit();
    end;
    '*': begin
      UpdateLookaheadToken(ttMULTIPLY, '*');
      Lookahead.Position := TokenPos;
      Exit();
    end;
    '/': begin
      UpdateLookaheadToken(ttDIVIDE, '/');
      Lookahead.Position := TokenPos;
      Exit();
    end;
    '(': begin
      UpdateLookaheadToken(ttLEFT_PARENS, '(');
      Lookahead.Position := TokenPos;
      Exit();
    end;
    ')': begin
      UpdateLookaheadToken(ttRIGHT_PARENS, ')');
      Lookahead.Position := TokenPos;
      Exit();
    end;
    ';': begin
      UpdateLookaheadToken(ttSEMICOLON, ';');
      Lookahead.Position := TokenPos;
      Exit();
    end;
    '{': begin
      UpdateLookaheadToken(ttCURLY_LEFT, '{');
      Lookahead.Position := TokenPos;
      Exit();
    end;
    '}': begin
      UpdateLookaheadToken(ttCURLY_RIGHT, '}');
      Lookahead.Position := TokenPos;
      Exit;
    end;
    '=': begin
      UpdateLookaheadToken(ttEQUAL_SIGN, '=');
      Lookahead.Position := TokenPos;
      Exit;
    end;
    '|': begin
      if PeekedChar = '|' then
      begin
        ReadChar();
        UpdateLookaheadToken(ttOR, '||');
        Lookahead.Position := TokenPos;
        Exit;
      end;
    end;
    '&': begin
      if PeekedChar = '&' then
      begin
        ReadChar();
        UpdateLookaheadToken(ttAND, '&&');
        Lookahead.Position := TokenPos;
        Exit;
      end;
    end;
    '': begin
      UpdateLookaheadToken(ttNONE, '');
      Lookahead.Position := TokenPos;
      Exit();
    end;
  end;
  {check for integer ttNUMBER }

  if IsDigit(utf8decode(CurrentChar), 1) then
  begin
    s := CurrentChar;
    while (PeekedChar <> '') and (IsDigit(utf8decode(PeekedChar), 1)) and
      (PeekedChar <> '.') do
    begin
      ReadChar();
      s := s + CurrentChar;
    end;
    {check for decimal ttNUMBER }
    if PeekedChar = '.' then
    begin
      ReadChar();
      s := s + CurrentChar;
      while (PeekedChar <> '') and (IsDigit(utf8decode(PeekedChar), 1)) do
      begin
        ReadChar();
        s := s + CurrentChar;
      end;
    end;
    if not (IsLetter(UTF8Decode(PeekedChar), 1)) then
    begin
      UpdateLookaheadToken(ttNUMBER, s);
      Lookahead.Position := TokenPos;
      Exit();
    end;

  end;

  {check for ttIDENTIFIER }
  if IsLetter(utf8decode(CurrentChar), 1) then
  begin
    s := CurrentChar;
    while (PeekedChar <> '') and (IsLetterOrDigit(UTF8Decode(PeekedChar), 1)) do
    begin
      ReadChar();
      s := s + CurrentChar;
    end;

    _Word := TWord(WordsTable.Items[s]);
    if _Word <> nil then   // ttIDENTIFIER exists ?
    begin
      UpdateLookaheadToken(_Word.Token.Tag, _Word.Token.Lexeme); //existing word to Lookahead
      AddWordPosition(TokenPos, _Word); // update word table entry
      Lookahead.Position := TokenPos;  //and   Lookahead position
    end
    else
    begin
      UpdateLookaheadToken(ttIDENTIFIER, s);
      _Token := TToken.Create(ttIDENTIFIER, s);
      _Word := TWord.Create(_Token);    // create copy of data as Lookahead will get changed
      AddWordPosition(TokenPos, _Word); // input position to new word
      Lookahead.Position := TokenPos;  //and  to new Lookahead
      WordsTable.Add(s, _Word);  // add new ttIDENTIFIER to WordsTable table
    end;

    {todo: finalize token storage for identifiers }
    Exit();
  end;
  UpdateLookaheadToken(ttUNKNOWN, CurrentChar);
  Exit();
end;

procedure TLexer.Match(checked_token: TTokenTag);
var
  token_name: string;
begin
  if checked_token = Lookahead.Tag then
  begin

    Advance();

  end
  else
  begin
    WriteStr(token_name, checked_token);
    raise Exception.Create('Syntax error in line:' + IntToStr(CurrentLineNumber) +
      LineEnding + token_name + ' expected' + LineEnding + CurrentLine);
  end;
end;

procedure TLexer.IsNumber(negative: boolean);
{ #done : solve negative ttNUMBER lexing}
begin
  if IsDigit(utf8decode(CurrentChar), 1) then
  begin
    if negative then Lookahead.Lexeme := '-' + CurrentChar
    else
      Lookahead.Lexeme := CurrentChar;
    while (PeekedChar <> '') and (IsDigit(utf8decode(PeekedChar), 1)) and
      (PeekedChar <> '.') do
    begin
      ReadChar();
      Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
    end;
    {check for decimal ttNUMBER }
    if PeekedChar = '.' then
    begin
      ReadChar();
      Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
      while (PeekedChar <> '') and (IsDigit(utf8decode(PeekedChar), 1)) do
      begin
        ReadChar();
        Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
      end;
    end;
    Lookahead.Tag := ttNUMBER;
    Exit();
  end;
end;

function TLexer.InitWordsTable: TFPObjectHashTable;
type
  TWordList = array [0..3] of string;
  TTagList = array [0..3] of TTokenTag;
var
  WordList: TWordList = ('num', 'bool', 'true', 'false');
  TagList: TTagList = (ttTYPENAME, ttTYPENAME, ttTRUE, ttFALSE);
  WordName: string;
  tag: TTokenTag;
  t: TToken;
  i: integer;
  word: TWord;
begin
  Result := TFPObjectHashTable.Create();
  for i := 0 to Length(WordList) - 1 do
  begin
    WordName := WordList[i];
    tag := TagList[i];
    t := TToken.Create(tag, WordName);
    word := TWord.Create(t);
    Result.Add(WordName, word);
  end;

end;

procedure TLexer.WordsIterator(Item: TObject; const key: string; var Continue: boolean);
var
  ar: array [0..1] of integer;
begin
  Continue := True;
  Write(key, ' : ');

  for ar in TWord(Item).Positions do
  begin
    //ttIDENTIFIER postions in src text line, col
    Write('(', ar[0], ',', ar[1], ') ');
  end;
  writeln();
end;

procedure TLexer.AddWordPosition(APosition: TPosition; AWord: TWord);
begin
  insert(APosition, AWord.Positions, MaxInt);
end;

{ TToken }

constructor TToken.Create(ATag: TTokenTag; ALexeme: string);
begin
  Tag := ATag;
  Lexeme := ALexeme;
end;

end.
