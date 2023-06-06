program compiler;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Forms, control_panel, Lexer, Parser;

{$R *.res}

begin
  {$IFDEF WINDOWS}
  AllocConsole;      // in Windows unit
  IsConsole := True; // in System unit
  SysInitStdIO;      // in System unit
  {$ENDIF}
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TControlPanel, ControlPanel);
  Application.Run;
end.

