{
    This file is part of Dev-C++
    Copyright (c) 2004 Bloodshed Software

    Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit Debugger;

interface

uses
  Sysutils, Windows, Messages, Forms, Classes, Controls,
  debugreader, version, editor, ComCtrls, Dialogs, MultiLangSupport;

type

  TEvalReadyEvent = procedure(const evalvalue: String) of object;

  TDebugger = class(TObject)
  private
    fOutputRead: THandle;
    fOutputWrite: THandle;
    fInputRead: THandle;
    fInputWrite: THandle;
    fProcessID: THandle;
    fExecuting: boolean;
    fCommandChanged: boolean;
    fDebugView: TTreeView;
    fLeftPageIndexBackup: integer;
    fBreakPointList: TList;
    fWatchVarList: TList;
    fOnEvalReady: TEvalReadyEvent;
    fReader: TDebugReader;
    fGdbBreakpointCounter: integer;
    function GetBreakPointFile: String;
    function GetBreakpointByGdbNum(var bp: PBreakPoint;
        bpNum: integer; bpFile: String): boolean;
    procedure ForgetBreakpointLine(bpLine: integer; bpFile: String);
  public
    constructor Create;
    destructor Destroy; override;

    // Play/pause
    procedure Start;
    procedure Stop;
    procedure SendCommand(const command, params: String; viewinui: boolean = false);

    // breakpoints
    procedure SendBreakPoint(i: integer);
    procedure RemoveBreakPoint(i: integer); overload;
	procedure AddBreakPoint(Linein: integer; e: TEditor);
    procedure RemoveBreakPoint(Linein: integer; e: TEditor); overload;
    procedure DeleteBreakPointsOf(editor: TEditor);
    procedure HandleBreakpointFeedback(gdbMsg: String);
    function NewBreakPoint(Linein: integer; e: TEditor): PBreakPoint;
    function GdbNextBreakpointNumber: integer;
    procedure GdbNumZeroToAllBreakpoints;

    // watch var
    procedure AddWatchVar(i: integer); overload;
    procedure RemoveWatchVar(i: integer); overload;
    procedure AddWatchVar(const namein: String); overload;
    procedure RemoveWatchVar(nodein: TTreeNode); overload;
    procedure RefreshWatchVars;
    procedure DeleteWatchVars(deleteparent: boolean);

    // Access
    property Executing: boolean read fExecuting write fExecuting;
    property LeftPageIndexBackup: integer read fLeftPageIndexBackup write fLeftPageIndexBackup;
    property WatchVarList: TList read fWatchVarList write fWatchVarList;
    property BreakPointList: TList read fBreakPointList write fBreakPointList;
    property CommandChanged: boolean read fCommandChanged write fCommandChanged;
    property DebugView: TTreeView read fDebugView write fDebugView;
    property OnEvalReady: TEvalReadyEvent read fOnEvalReady write fOnEvalReady;
    property Reader: TDebugReader read fReader write fReader;
    property BreakPointFile: String read GetBreakPointFile;
  end;

implementation

uses
  System.UItypes, main, devcfg, utils, cpufrm;

constructor TDebugger.Create;
begin
  inherited;
  BreakPointList := TList.Create;
  WatchVarList := TList.Create;
end;

destructor TDebugger.Destroy;
var
  I: integer;
begin
  Stop;

  // Remove watch vars
  for i := 0 to WatchVarList.Count - 1 do
    Dispose(PWatchVar(WatchVarList.Items[i]));
  WatchVarList.Free;

  // Remove the breakpoints
  for i := 0 to BreakPointList.Count - 1 do
    Dispose(PBreakPoint(BreakPointList.Items[i]));
  BreakPointList.Free;

  inherited;
end;

procedure TDebugger.Start;
var
  pi: TProcessInformation;
  si: TStartupInfo;
  sa: TSecurityAttributes;
  GDBFile, GDBCommand: String;
  CompilerSet: TdevCompilerSet;
begin
  Executing := true;
  fGdbBreakpointCounter := 0;

  // Set up the security attributes struct.
  sa.nLength := sizeof(TSecurityAttributes);
  sa.lpSecurityDescriptor := nil;
  sa.bInheritHandle := true;

  // Create the child output pipe.
  if not CreatePipe(fOutputread, fOutputwrite, @sa, 0) then
    Exit;
  if not SetHandleInformation(fOutputread, HANDLE_FLAG_INHERIT, 0) then
    Exit;

  // Create the child input pipe.
  if not CreatePipe(fInputread, fInputwrite, @sa, 0) then
    Exit;
  if not SetHandleInformation(fInputwrite, HANDLE_FLAG_INHERIT, 0) then
    Exit;

  // Set up the start up info struct.
  FillChar(si, sizeof(TStartupInfo), 0);
  si.cb := sizeof(TStartupInfo);
  si.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW or STARTF_USESHOWWINDOW;
  si.hStdInput := fInputread;
  si.hStdOutput := fOutputwrite;
  si.hStdError := fOutputwrite;
  si.wShowWindow := SW_HIDE;

  // Use the GDB provided in the project if needed
  CompilerSet := devCompilerSets.CompilationSet;

  // Assume it's present in the first bin dir
  if CompilerSet.BinDir.Count > 0 then begin
    GDBFile := CompilerSet.BinDir[0] + pd + CompilerSet.gdbName;
    GDBCommand := '"' + GDBFile + '"' + ' --annotate=2 --silent';
    if not CreateProcess(nil, PChar(GDBCommand), nil, nil, true, CREATE_NEW_CONSOLE, nil, nil, si, pi) then begin
      MessageDlg(Format(Lang[ID_ERR_ERRORLAUNCHINGGDB], [GDBFile, SysErrorMessage(GetLastError)]), mtError,
        [mbOK], 0);
      Executing := false;
      Exit;
    end;
  end else
    MessageDlg(Lang[ID_ERR_GDBNOUTFOUND], mtError, [mbOK], 0);

  fProcessID := pi.hProcess;

  // Create a thread that will read GDB output.
  Reader := TDebugReader.Create(true);
  Reader.PipeRead := fOutputRead;
  Reader.FreeOnTerminate := true;
  Reader.BreakpointList := BreakPointList;
  Reader.WatchVarList := WatchVarList;
  Reader.DebugView := DebugView;
  Reader.Start;

  MainForm.UpdateAppTitle;

  Application.HintHidePause := 5000;
end;

procedure TDebugger.Stop;
begin
  if Executing then begin
    Executing := false;

    if WatchVarList.Count = 0 then // nothing worth showing, restore view
      MainForm.LeftPageControl.ActivePageIndex := LeftPageIndexBackup;

    // Close CPU window
    if Assigned(CPUForm) then
      CPUForm.Close;

    TerminateProcess(fProcessID, 0); // stop gdb

    Reader.Terminate;
    Reader := nil;

    // Free resources
    if not CloseHandle(fProcessID) then
      Exit;
    if not CloseHandle(fOutputwrite) then
      Exit;
    if not CloseHandle(fInputread) then
      Exit;

    MainForm.RemoveActiveBreakpoints;

    MainForm.UpdateAppTitle;

    Application.HintHidePause := 2500;
  end;
end;

procedure TDebugger.SendCommand(const Command, Params: String; ViewInUI: boolean);
var
  nBytesWrote: DWORD;
  Buff: TBytes;
begin
  if Executing then
  begin
    Buff := TEncoding.ANSI.GetBytes((command + ' ' + params).Trim) + [10];  // LF only thus

    if not WriteFile(fInputwrite, (@Buff[Low(Buff)])^, Length(Buff), nBytesWrote, nil) then
      MessageDlg(Lang[ID_ERR_WRITEGDB], mtError, [mbOK], 0);

    if ViewInUI then
      if (not CommandChanged) or (MainForm.edGdbCommand.Text = '') then begin
        // Convert command to C string
        if Length(params) > 0 then
          MainForm.edGdbCommand.Text := Command + ' ' + params
        else
          MainForm.edGdbCommand.Text := Command;

        CommandChanged := false;
      end;
  end;
end;

function TDebugger.GetBreakPointFile: String;
begin
  if Executing then
    Result := fReader.BreakPointFile
  else
    Result := '';
end;

function BpToString(var bp: PBreakPoint): String;
begin
  if Assigned(bp) then
    // don't trust bp^.inrternalindex here
    Result := '   bp:{line:' + IntToStr(bp^.line)
      + ', gdbNum:' + IntToStr(bp^.gdbNumber)
      + ', gdbAt:' + bp^.gdbAt
      + ', file:' + bp^.editor.FileName
      + '}'
  else
    Result := '';
end;


{ gdb doesn't neccesarily accept the given line number, but
    sets a stop somewhere near...
    Lets say you clicked on lines 149 and 155 before starting the debug session.
    When gdb starts (by F5 or...), it reports during startup:
@post-prompt
Breakpoint 1 at 0x402daa: file CompareDir.cpp, line 150.
@breakpoints-invalid
@pre-prompt
(gdb)
@prompt
@post-prompt
Breakpoint 2 at 0x402eca: file CompareDir.cpp, line 161.
@breakpoints-invalid
@pre-prompt
(gdb)
@prompt

    and little bit later:

@starting
...
@breakpoint 1
Thread 1 hit Breakpoint 1,
@frame-begin 0 0x402daa    <-- as it said, stop at line 150
@frame-function-name
main

    When the instruction to gdb results in colliding breaks at the same place,
    then gdb sends a Note in @post-prompt complaining about it:

@post-prompt
Note: breakpoint 1 also set at pc 0x402eca.
Breakpoint 2 at 0x402eca: file CompareDir.cpp, line 161.

TODO: what happens then if one of colliding stops is removed in gdb,
will others stay active?

}

function TDebugger.GdbNextBreakpointNumber: integer;
begin
  inc(fGdbBreakpointCounter);
  Result := fGdbBreakpointCounter;
end;

function TDebugger.GetBreakpointByGdbNum(var bp: PBreakPoint;
  bpNum: integer; bpFile: String): boolean;
var
  ii: integer;
begin
  Result := false;
  for ii := 0 to pred(fBreakPointList.Count) do
  begin
    bp := PBreakPoint(BreakPointList.Items[ii]);
    if (abs(bp^.gdbNumber) = bpNum) then
    begin
      // check if we are in the same file, otherways... skip for now
      if (bpFile = '') or (pos(bpFile, bp^.editor.FileName) > 0) then
      begin
        Result := true;
        bp^.gdbNumber := bpNum; // confirmed
        Exit;
      end;
    end;
  end;
end;


procedure TDebugger.HandleBreakpointFeedback(gdbMsg: String);
var
  ii, px, bpNum, bpLine: integer;
  sx, bpFile, bpAt: String;
  bp, bptmp: PBreakPoint;
  tok: TStringList;
  collidings: Array of Integer;
  edx: TEditor;

  function mend(sx: String): String;
  var c: char; i: integer;
  begin
    Result := Trim(sx);
    i := Length(Result);
    if i = 0 then
      Exit;
    c := Result[i];
    if CharInSet(c, [':', '.', ',']) then
       SetLength(Result, i - 1);
  end;

  procedure ParseCollidingBreakNums;
  var
    i, num: integer;
  begin
    // 8 9     10          11 ? ?+1      ?-5
    // | Note: breakpoints 1, 2, 3 and 4 also set at pc 0x402eca.
    // gather numbers between words 'breakpoint' and 'also'
    if StartsText('breakpoint', tok[10]) then
    begin
      for i := 11 to tok.Count - 3 do
      begin
        if tok[i] = 'also' then
          Exit;
        num := StrToIntDef(mend(tok[i]), -1);
        if num > 0 then
        begin
          SetLength(collidings, 1 + Length(collidings));
          collidings[Length(collidings) - 1] := num;
        end;
      end;
    end;
  end;


begin
        //FIXME split smth, too many functions here together

MainForm.DebugOutput.Lines.Append('>RepaintBreakpoints() got= ' + gdbMsg);
  tok := TStringList.Create;
  tok.Delimiter := ' ';
  tok.StrictDelimiter := True;
  tok.DelimitedText := gdbMsg;
  bpNum := 0; //just to init ?

  // Deleted beakpoint 1
  if (tok.Count > 2) and (tok[0] = 'Deleted') then
  begin
    bpNum := StrToIntDef(tok[2], -1);
    if GetBreakpointByGdbNum(bptmp, bpNum, bpFile) then
    begin
      // gdb Numbers 1,2,3.. for beakpoints just keep growing, passivate our existing
      bptmp^.gdbAt := 'Deleted';
      bptmp^.editor.RepaintBreakpoints(bptmp^.line);
      MainForm.DebugOutput.Lines.Append('>>RepaintBreakpoints() for Deleted ' + BpToString(bp));
    end
    else
    begin
      MainForm.DebugOutput.Lines.Append('>SKIP Deleted, no match by num '
        + IntToStr(bpNum));
    end;

    Exit;
  end;

  // No breakpoint at "CompareDir.cpp":189.
  if StartsStr('No breakpoint at', gdbMsg) then
  begin
    px := RPos(':', gdbMsg);
    if px > 0 then
    begin
      sx := mend(Copy(gdbMsg, px+1, Length(gdbMsg) - px));
      bpLine := StrToIntDef(sx, -1);
      SetLength(gdbMsg, px-1);
      bpFile := Copy(gdbMsg, 18, px-1); // full filepath as in command 'clear ...'
      bpFile := ExtractFileName(bpFile.DeQuotedString('"').Replace('/', '\')); //   slash \/ issue
      MainForm.Debugger.ForgetBreakpointLine(bpLine, bpFile);
    end
    else
      MainForm.DebugOutput.Lines.Append('>SKIP Deleted, no ":" ');
    Exit;
  end;

  if tok.Count < 8 then  // some other format, pass
  begin
    MainForm.DebugOutput.Lines.Append('>>SKIP RepaintBreakpoints() too few tokens ');
    Exit;
  end;

  // 0          1 2  3         4    5               6    7
  // Breakpoint 1 at 0x402cb1: file CompareDir.cpp, line 140
  if tok[0] = 'Breakpoint' then
  begin
    bpNum := StrToIntDef(tok[1], 1);
    bpAt := mend(tok[3]);
    bpFile := mend(tok[5]);
    sx := mend(tok[7]);
    bpLine := StrToIntDef(sx, -1);
    if bpLine < 0 then // can't use that one
    begin
      MainForm.DebugOutput.Lines.Append('>>SKIP RepaintBreakpoints() bad-parse [7] ');
      Exit;
    end;
  end
  // 0         1          2 3  4         5    6               7    8
  // Temporary breakpoint 1 at 0x402cb1: file CompareDir.cpp, line 140
  else
  if tok[0] = 'Temporary' then
  begin
    sx := mend(tok[2]);
    bpNum := StrToIntDef(sx, -1);
    if bpNum < 0 then // can't use that one
    begin
      MainForm.DebugOutput.Lines.Append('>>SKIP RepaintBreakpoints() bad-parse [2]');
      Exit;
    end;
    fGdbBreakpointCounter := bpNum;
    MainForm.DebugOutput.Lines.Append('>> RepaintBreakpoints() fGdbBreakpointCounter:= '
    + IntToStr(fGdbBreakpointCounter));
    Exit;
  end;

  // got Note: ...
  if tok.Count > 14 then
  begin
    if tok[8] = '|' then // | is specially inserted
    begin
      MainForm.DebugOutput.Lines.Append('>>Handling Note: ');
      ParseCollidingBreakNums;

      for ii := 0 to Length(collidings) - 1 do
      begin
        if GetBreakpointByGdbNum(bptmp, collidings[ii], bpFile) then
        begin
          bptmp^.gdbAt := 'hide_' + bpAt;
          bptmp^.editor.RepaintBreakpoints(bptmp^.line);
          bptmp^.editor.RepaintBreakpoints(bpLine); // invalidate both lines  FIXME bpLine not initialized?
          MainForm.DebugOutput.Lines.Append('>> RepaintBreakpoints() hide ' + BpToString(bptmp));
        end;
      end;
    end;
  end;

  if GetBreakpointByGdbNum(bp, bpNum, bpFile) then  //FIXME bpNum not initialized?
  begin
    if bpLine <> bp^.line then // must update Editor
    begin
      px := bp^.line;
      bp^.line := bpLine;  // move to line given by gdb
      bp^.editor.RepaintBreakpoints(px);
      bp^.editor.RepaintBreakpoints(bpLine); // invalidate both lines

      MainForm.DebugOutput.Lines.Append('>> move from line:'
        + IntToStr(px) + ' to:' + IntToStr(bpLine) + ' >>  ' + BpToString(bp));
    end
    else
      MainForm.DebugOutput.Lines.Append('>>> RepaintBreakpoints() same line ');
  end
  else
  begin
    // new breakpoint reported
    MainForm.DebugOutput.Lines.Append('>>> RepaintBreakpoints() is new? ');
    for ii := 0 to pred(MainForm.EditorList.PageCount) do
    begin
      edx := MainForm.EditorList.Editors[ii];
      if ContainsText(edx.FileName, bpFile) then
      begin
        bp := MainForm.Debugger.NewBreakPoint(bpLine, edx);
        bp^.gdbNumber := bpNum;
        fGdbBreakpointCounter := bpNum; // try to keep in sync
        bp^.editor.RepaintBreakpoints(bpLine);
        MainForm.DebugOutput.Lines.Append('>>> RepaintBreakpoints() new added ' + BpToString(bp) );
        Exit;
      end;
    end;

    MainForm.DebugOutput.Lines.Append('>>>SKIP RepaintBreakpoints() for ' + gdbMsg);

  end;

end;

/// send 'break' command to gdb, when debug session starts | .toggle()
procedure TDebugger.SendBreakPoint(i: integer);
var
  filename, parm: String;
  bp: PBreakPoint;
begin
  // break "filename":linenum
  bp := PBreakPoint(BreakPointList.Items[i]);
  // using calling order, assuming gdb does the same
  if bp^.gdbNumber = 0 then // we're using confirmed/unconfirmed +- logic
    bp^.gdbNumber := -GdbNextBreakpointNumber;
  bp^.gdbAt := ''; // empty means ok, no collision with some other breakpoint

  filename := StringReplace(bp^.editor.FileName, '\', '/', [rfReplaceAll]);
  parm :=  '"' + filename + '":' + inttostr(bp.line);
  SendCommand('break', parm, false);
  MainForm.DebugOutput.Lines.Append('>>SendBreakPoint(i:'
    + IntToStr(i) + ', g:' + IntToStr(bp^.gdbNumber) + ').cmd= break ' + parm);
end;

/// send 'clear' command to gdb
procedure TDebugger.RemoveBreakPoint(i: integer);
var
  filename, parm: String;
  bp: PBreakPoint;
begin
  bp := PBreakPoint(BreakPointList.Items[i]);

  // "filename":linenum
  filename := StringReplace(bp^.editor.FileName, '\', '/', [rfReplaceAll]);
  parm := '"' + filename + '":' + inttostr(bp^.line);
  SendCommand('clear', parm, true);
  MainForm.DebugOutput.Lines.Append('>>RemoveBreakPoint(internal:'
    + IntToStr(i) + ')>> clear ' + parm + BpToString(bp));
end;

function TDebugger.NewBreakPoint(linein: integer; e: TEditor): PBreakPoint;
begin
  Result := new(PBreakPoint);
  with Result^ do begin
    line := Linein;
    editor := e;
    gdbAt := '';
    gdbNumber := -GdbNextBreakpointNumber; // - means 'unconfirmed'
  end;
  BreakPointList.Add(Result);
end;

/// on gutter click, on .toggle()
procedure TDebugger.AddBreakPoint(Linein: integer; e: TEditor);
begin
	NewBreakPoint(linein, e);
  // Debugger already running? Add it to GDB
  if Executing then
    SendBreakPoint(BreakPointList.Count - 1);
end;

procedure TDebugger.GdbNumZeroToAllBreakpoints;
var
  i: integer;
  pb: PBreakPoint;
begin
  fGdbBreakpointCounter := 0;
  for i := 0 to BreakPointList.Count - 1 do
  begin
    pb := PBreakPoint(BreakPointList.Items[i]);
    pb^.gdbNumber := 0;
  end;
end;

procedure TDebugger.RemoveBreakPoint(Linein: integer; e: TEditor);
var
  i: integer;
  pb: PBreakPoint;
begin
  for i := 0 to BreakPointList.Count - 1 do
  begin
    pb := PBreakPoint(BreakPointList.Items[i]);
    if (pb^.line = Linein) and (pb^.editor = e) then
    begin

      // Debugger already running? Remove it from GDB if real
      if Executing then
        RemoveBreakPoint(i);

      // Remove from list
      Dispose(PBreakPoint(BreakPointList.Items[i]));
      BreakPointList.Delete(i);
      break;
    end;
  end;
end;

procedure TDebugger.DeleteBreakPointsOf(editor: TEditor);
var
  I: integer;
begin
  // Breakpoints in closed files need to be deleted
  for i := BreakPointList.Count - 1 downto 0 do

    if PBreakPoint(BreakPointList.Items[i])^.editor = editor then begin

      // Remove from list
      Dispose(PBreakPoint(BreakPointList.Items[i]));
      BreakPointList.Delete(i);
    end;
end;

procedure TDebugger.ForgetBreakpointLine(bpLine: integer; bpFile: String);
var
  i: integer;
  onn: boolean;
  bp: PBreakPoint;
  editor: TEditor;
begin
  for i := BreakPointList.Count - 1 downto 0 do
  begin
    bp := PBreakPoint(BreakPointList.Items[i]);
    onn := bp^.editor.FileName.Contains(bpFile);
    if (bp^.line = bpLine)
    and (onn) then
    begin
      editor := bp^.editor;

      // Remove from list
      Dispose(PBreakPoint(BreakPointList.Items[i]));
      BreakPointList.Delete(i);
      editor.RepaintBreakpoints(bpLine);
    end;
  end;
end;

procedure TDebugger.AddWatchVar(i: integer);
begin
  SendCommand('display', PWatchVar(WatchVarList.Items[i])^.name, true);
end;

procedure TDebugger.RemoveWatchVar(i: integer);
begin
  SendCommand('undisplay', IntToStr(PWatchVar(WatchVarList.Items[i])^.gdbindex), true);
end;

procedure TDebugger.AddWatchVar(const namein: String);
var
  parentnode: TTreeNode;
  I: integer;
  wparent: PWatchVar;
begin

  // Don't allow duplicates...
  for I := 0 to WatchVarList.Count - 1 do
    if SameStr(PWatchVar(WatchVarList.Items[i])^.name, namein) then
      Exit;

  // Add parent to list
  wparent := New(PWatchVar);
  wparent^.name := namein;
  //	wparent^.value := 'Execute to evaluate';
  wparent^.gdbindex := -1; // filled by GDB
  WatchVarList.Add(wparent);

  // Add parent to GUI
  parentnode := DebugView.Items.AddObject(nil, wparent^.name + ' = Execute to evaluate', wparent);
  parentnode.ImageIndex := 21;
  parentnode.SelectedIndex := 21;

  // Refer to list from GUI
  wparent^.node := parentnode;

  // Debugger already running? Add it to GDB
  if Executing then
    AddWatchVar(WatchVarList.Count - 1);
end;

procedure TDebugger.RemoveWatchVar(nodein: TTreeNode);
var
  I: integer;
  wparent: PWatchVar;
begin
  for i := 0 to WatchVarList.Count - 1 do begin
    wparent := PWatchVar(WatchVarList.Items[I]);

    if SameStr(wparent^.name, PWatchVar(nodein.Data)^.name) then begin

      // Debugger already running and GDB scanned this one? Remove it from GDB
      if Executing and (wparent^.gdbindex <> -1) then
        RemoveWatchVar(i);

      // Remove from UI
      nodein.DeleteChildren;
      nodein.Delete;

      // Remove from list
      Dispose(PWatchVar(WatchVarList.Items[i]));
      WatchVarList.Delete(i);

      break;
    end;
  end;
end;

procedure TDebugger.RefreshWatchVars;
var
  I: integer;
begin
  // Variables that aren't found need to be re-displayed!
  for i := 0 to WatchVarList.Count - 1 do
    if PWatchVar(WatchVarList.Items[i])^.gdbindex = -1 then
      AddWatchVar(i); // resends command to display to GDB
end;

procedure TDebugger.DeleteWatchVars(deleteparent: boolean);
var
  I: integer;
  wparent: PWatchVar;
begin
  DebugView.Items.BeginUpdate;
  try
    for I := WatchVarList.Count - 1 downto 0 do begin
      wparent := PWatchVar(WatchVarList.Items[I]);

      if deleteparent then begin

        // Remove from UI
        if wparent^.node.HasChildren then
          wparent^.node.DeleteChildren;
        wparent^.node.Delete;

        // Remove from list
        Dispose(PWatchVar(WatchVarList.Items[i]));
        WatchVarList.Delete(i);
      end else begin

        // Remove from UI
        if wparent^.node.HasChildren then
          wparent^.node.DeleteChildren;

        // Leave parent node intact...
        wparent^.gdbindex := -1;
        wparent^.node.Text := wparent^.name + ' = Execute to evaluate';
      end;
    end;
  finally
    DebugView.Items.EndUpdate;
  end;
end;

end.

