unit Lexer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Contnrs;

type
  TTokenTag = (ttNONE, ttNUMBER, ttIDENTIFIER, ttPLUS, ttMINUS, ttMULTIPLY,
    ttDIVIDE, ttLEFT_PARENS,
    ttRIGHT_PARENS, ttSEMICOLON, ttFILE_END, ttUNKNOWN, ttCURLY_LEFT, ttCURLY_RIGHT,
    ttTYPENAME, ttEQUAL_SIGN, ttAND, ttOR, ttIS_EQUAL, ttTRUE, ttFALSE);

  { TToken }

  TToken = class
    Tag: TTokenTag;
    Lexeme: string;
    Value: extended;
    LexemePosition: array of array [0..1] of integer;

    constructor Create(ATag: TTokenTag; ALexeme: string);
  end;


  { TLexer }

  TLexer = class
  type
    TPosition = array [0..1] of integer;

  private
    FLookahead: TToken;
    procedure setLookahead(AValue: TToken);
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
    Words: TFPObjectHashTable;

    constructor Create(const SrcFileName: string);
    constructor CreateManual(AMemoStrings: TStrings);
    destructor Destroy; override;
    function ReadLine(): boolean;
    function PeekLine(src: TStringList): boolean;
    procedure ReadChar();
    procedure PeekChar();
    function PeekCharNonWhite(): string;
    procedure Advance();
    procedure Match(checked_token: TTokenTag);
    procedure IsNumber(negative: boolean);
    function InitWordsTable: TFPObjectHashTable;
    procedure WordsIterator(Item: TObject; const key: string; var Continue: boolean);
    procedure StoreLexemePosition(APosition: TPosition; AToken: TToken);
    property Lookahead: TToken read FLookahead write setLookahead;


  end;



implementation

uses
  SysUtils, LazUTF8, Character;

procedure TLexer.setLookahead(AValue: TToken);
begin
  if FLookahead = AValue then Exit;
  FreeAndNil(FLookahead);
  FLookahead := AValue;
end;

constructor TLexer.Create(const SrcFileName: string);
begin
  SrcLines := TStringList.Create;
  SrcLines.LoadFromFile(SrcFileName);
  Lookahead := TToken.Create(ttNONE, '');
  Words := InitWordsTable;
end;

constructor TLexer.CreateManual(AMemoStrings: TStrings);
begin
  SrcLines := TStringList.Create;
  SrcLines.AddStrings(AMemoStrings);
  Lookahead := TToken.Create(ttNONE, '');
  Words := InitWordsTable;

end;

destructor TLexer.Destroy;
begin
  FreeAndNil(SrcLines);
  FreeAndNil(FLookahead);
  FreeAndNil(Words);
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
    { #todo : peekchar how about if next line is just newline ? }
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
  t: TToken = nil;
  s: string = '';
  IdPosition: array [0..1] of integer;

begin
  ReadChar();

  while (CurrentChar <> '') and (IsWhiteSpace(utf8decode(CurrentChar), 1) or
      (CurrentChar = LineEnding)) do
    ReadChar();

  IdPosition[0] := CurrentLineNumber;
  IdPosition[1] := CharPosition;
  case CurrentChar of
    '+': begin
      Lookahead := TToken.Create(ttPLUS, '+');
      Exit();
    end;
    '-': begin
      Lookahead := TToken.Create(ttMINUS, '-');
      Exit();
    end;
    '*': begin
      Lookahead := TToken.Create(ttMULTIPLY, '*');
      Exit();
    end;
    '/': begin
      Lookahead := TToken.Create(ttDIVIDE, '/');
      Exit();
    end;
    '(': begin
      Lookahead := TToken.Create(ttLEFT_PARENS, '(');
      Exit();
    end;
    ')': begin
      Lookahead := TToken.Create(ttRIGHT_PARENS, ')');
      Exit();
    end;
    ';': begin
      Lookahead := TToken.Create(ttSEMICOLON, ';');
      Exit();
    end;
    '{': begin
      Lookahead := TToken.Create(ttCURLY_LEFT, '{');
      Exit();
    end;
    '}': begin
      Lookahead := TToken.Create(ttCURLY_RIGHT, '}');
      Exit;
    end;
    '=': begin
      Lookahead := TToken.Create(ttEQUAL_SIGN, '=');
      Exit;
    end;
    '|': begin
      if PeekedChar = '|' then
      begin
        ReadChar();
        Lookahead := TToken.Create(ttOR, '||');
      end;
      Exit;
    end;
    '&': begin
      if PeekedChar = '&' then
      begin
        ReadChar();
        Lookahead := TToken.Create(ttAND, '&&');
      end;
      Exit;
    end;
    '': begin
      Lookahead := TToken.Create(ttNONE, '');
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
      Lookahead := TToken.Create(ttNUMBER, s);
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

    t := TToken(Words.Items[s]);
    if t <> nil then   // ttIDENTIFIER exists ?
    begin
      Lookahead := TToken.Create(t.tag, s);
      StoreLexemePosition(IdPosition, t); // update word table entry
      StoreLexemePosition(IdPosition, Lookahead);  //and  to new Lookahead
    end
    else
    begin
      Lookahead := TToken.Create(ttIDENTIFIER, s);
      t := TToken.Create(ttIDENTIFIER, s);
      // create copy of data as Lookahead will get changed
      StoreLexemePosition(IdPosition, t); // update word table entry
      StoreLexemePosition(IdPosition, Lookahead);  //and  to new Lookahead
      Words.Add(s, t);  // add new ttIDENTIFIER to words table
    end;

    {todo: finalize token storage for identifiers }
    Exit();
  end;
  Lookahead.Tag := ttUNKNOWN;
  Lookahead.Lexeme := CurrentChar;
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
{ #todo : solve negative ttNUMBER lexing}
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
  s: string;
  tag: TTokenTag;
  t: TToken;
  i: integer;
begin
  Result := TFPObjectHashTable.Create();
  for i := 0 to Length(WordList) - 1 do
  begin
    s := WordList[i];
    tag := TagList[i];
    t := TToken.Create(tag, s);
    Result.Add(s, t);
  end;

end;

procedure TLexer.WordsIterator(Item: TObject; const key: string; var Continue: boolean);
var
  ar: array [0..1] of integer;
begin
  Continue := True;
  Write(key, ' : ');

  for ar in TToken(Item).LexemePosition do
  begin
    //ttIDENTIFIER postions in src text line, col
    Write('(', ar[0], ',', ar[1], ') ');
  end;
  writeln();
end;

procedure TLexer.StoreLexemePosition(APosition: TPosition; AToken: TToken);
begin
  insert(APosition, AToken.LexemePosition, MaxInt);
end;



{ TToken }

constructor TToken.Create(ATag: TTokenTag; ALexeme: string);
begin
  Tag := ATag;
  Lexeme := ALexeme;
  if ATag = ttNUMBER then
    Value := StrToFloat(Lexeme);

end;

end.
