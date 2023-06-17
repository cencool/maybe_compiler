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
  sysutils,
  Forms, control_panel, Lexer, Parser;

{$R *.res}

begin
  {$IFDEF WINDOWS}
  AllocConsole;      // in Windows unit
  IsConsole := True; // in System unit
  SysInitStdIO;      // in System unit
  {$ENDIF}

  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');

  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TControlPanel, ControlPanel);
  Application.Run;
end.

