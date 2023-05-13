unit control_panel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, Parser;

type

  { TControlPanel }

  TControlPanel = class(TForm)
    BCompileFile: TButton;
    BCompileText: TButton;
    Label1: TLabel;
    ManualInput: TMemo;
    ParseTreeView: TTreeView;
    procedure BCompileFileClick(Sender: TObject);
    procedure BCompileTextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private

  public

  end;

var
  ControlPanel: TControlPanel;

implementation

{$R *.lfm}

{ TControlPanel }

procedure TControlPanel.BCompileFileClick(Sender: TObject);
const
  filename = 'in.txt';
var
  MyParser: TParser;
begin
  try
    MyParser := TParser.Create;
    MyParser.parse(filename);
    Writeln('PARSE TREE:');
    MyParser.PrintParseTree(MyParser.ParseRoot);
    WriteLn();
    MyParser.ShowParseTree(MyParser.ParseRoot, ParseTreeView);
    Writeln('SYNTAX TREE:');
    MyParser.PrintSyntaxTree(MyParser.SyntaxRoot);
    WriteLn();
    Writeln('TRANSLATION TO POSTFIX');
    MyParser.SyntaxRoot.Gen;
    Writeln();
    FreeAndNil(MyParser);
  except
    on E: Exception do
    begin
      Writeln('Compiler exited with Error');
    end;
  end;
end;

procedure TControlPanel.BCompileTextClick(Sender: TObject);
var
  MyParser: TParser;
begin
  try
    MyParser := TParser.Create;
    MyParser.parseManual(ManualInput.Lines);
    Writeln('PARSE TREE:');
    MyParser.PrintParseTree(MyParser.ParseRoot);
    WriteLn();
    MyParser.ShowParseTree(MyParser.ParseRoot, ParseTreeView);
    Writeln('SYNTAX TREE:');
    MyParser.PrintSyntaxTree(MyParser.SyntaxRoot);
    WriteLn();
    Writeln('TRANSLATION TO POSTFIX');
    MyParser.SyntaxRoot.Gen;
    Writeln();
    FreeAndNil(MyParser);
  except
    on E: Exception do
    begin
      Writeln('Compiler exited with Error');
    end;
  end;
end;

procedure TControlPanel.FormCreate(Sender: TObject);
begin
  ManualInput.Caption := '';
end;

procedure TControlPanel.Label1Click(Sender: TObject);
begin

end;

end.
