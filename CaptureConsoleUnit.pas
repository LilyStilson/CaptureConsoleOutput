unit CaptureConsoleUnit;

interface

uses
  System.Classes, System.SysUtils, System.Threading,
  {$IFDEF MSWINDOWS}
  Winapi.Windows;
  {$ELSE POSIX}
  Posix.Base;
  {$ENDIF}

type
  TConsoleCallback<T> = reference to procedure(const Arg: T);
  procedure CaptureConsoleOutputSync(const Command: String; Callback: TConsoleCallback<String>);

implementation

/// <summary>
/// Calls system console and redirects it's lines to the <para>Callback</para>
/// </summary>
/// <param name="ACommand">Application or Command to execute</param>
/// <param name="AParameters">Optional argumets</param>
/// <param name="Callback">Callback procedure (Line: String)</param>
/// <remarks>
/// Because of TThread.Synchronize() this procedure should be used in an asyncronous context
/// </remarks>
procedure CaptureConsoleOutputSync(const Command: String; Callback: TConsoleCallback<String>);
{$IFDEF MSWINDOWS}
const
  CReadBuffer = 1;
var
  saSecurity: TSecurityAttributes;
  hRead, hWrite: THandle;
  suiStartup: TStartupInfo;
  piProcess: TProcessInformation;
  pBuffer, dBuffer: array [0..CReadBuffer] of AnsiChar;
  dRead, dRunning, dAvailable: DWORD;
  TempString: String;
begin
  TempString := '';
  saSecurity.nLength := SizeOf(TSecurityAttributes);
  saSecurity.bInheritHandle := True;
  saSecurity.lpSecurityDescriptor := nil;

  if CreatePipe(hRead, hWrite, @saSecurity, 0) then
    try
      FillChar(suiStartup, SizeOf(TStartupInfo), #0);
      suiStartup.cb := SizeOf(TStartupInfo);
      suiStartup.hStdInput := hRead;
      suiStartup.hStdOutput := hWrite;
      suiStartup.hStdError := hWrite;
      suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      suiStartup.wShowWindow := SW_HIDE;

      if CreateProcess(nil, PChar('cmd /c "' + Command + '"'), @saSecurity, @saSecurity, true, NORMAL_PRIORITY_CLASS, nil, nil, suiStartup, piProcess) then
        try
          repeat
            dRunning := WaitForSingleObject(piProcess.hProcess, 1);
            PeekNamedPipe(hRead, nil, 0, nil, @dAvailable, nil);
            if dAvailable > 0 then begin
              repeat
                dRead := 0;
                ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
                pBuffer[dRead] := #0;
                OemToCharA(pBuffer, dBuffer);

                if (dBuffer[0] = #$D) then begin
                  if TempString.Contains(#$A) then begin
                    TempString := TempString.Replace(#$A, '');
                  end;
                  TThread.Synchronize(nil, procedure begin
                    Callback(TempString);
                  end);
                  TempString := '';
                end else
                  TempString := TempString + dBuffer[0];

              until dRead < CReadBuffer;
            end;
            //Application.ProcessMessages;
          until dRunning <> WAIT_TIMEOUT;
        finally
          CloseHandle(piProcess.hProcess);
          CloseHandle(piProcess.hThread);
        end;
    finally
      CloseHandle(hRead);
      CloseHandle(hWrite);
    end;
{$ELSE POSIX}
type
  PIOFile = Pointer;
  _popen = function (const Command, Modes: PAnsiChar): PIOFile; cdecl;
  _pclose = function (Stream: PIOFile): Integer; cdecl;
  _feof = function (Strean: PIOFile): Integer; cdecl;
  _fread = function (Ptr: Pointer; Size, N: LongWord; Stream: PIOFile): LongWord; cdecl;
  _wait = function (__stat_loc: PInteger): Integer; cdecl;

  function popen(const Command, Modes: PAnsiChar): PIOFile;
  var
    Handle: HMODULE;
    func: _popen;
  begin
    Result := nil;
    Handle := LoadLibrary('libc.dylib');
    if Handle <> 0 then begin
      try
        @func := GetProcAddress(Handle, _PU + 'popen');
        if @func <> nil then
          Result := func(Command, Modes);
      finally
        FreeLibrary(Handle);
      end;
    end;
  end;

  function pclose(Stream: PIOFile): Integer;
  var
    Handle: HMODULE;
    func: _pclose;
  begin
    Result := -1;
    Handle := LoadLibrary('libc.dylib');
    if Handle <> 0 then begin
      try
        @func := GetProcAddress(Handle, _PU + 'pclose');
        if @func <> nil then
          Result := func(Stream);
      finally
        FreeLibrary(Handle);
      end;
    end;
  end;

  function fread(Ptr: Pointer; Size, N: LongWord; Stream: PIOFile): LongWord;
  var
    Handle: HMODULE;
    func: _fread;
  begin
    Result := 0;
    Handle := LoadLibrary('libc.dylib');
    if Handle <> 0 then begin
      try
        @func := GetProcAddress(Handle, _PU + 'fread');
        if @func <> nil then
          Result := func(Ptr, Size, N, Stream);
      finally
        FreeLibrary(Handle);
      end;
    end;
  end;

  function feof(Stream: PIOFile): Integer;
  var
    Handle: HMODULE;
    func: _feof;
  begin
    Result := -1;
    Handle := LoadLibrary('libc.dylib');
    if Handle <> 0 then begin
      try
        @func := GetProcAddress(Handle, _PU + 'feof');
        if @func <> nil then
          Result := func(Stream);
      finally
        FreeLibrary(Handle);
      end;
    end;
  end;

  function wait(__stat_loc: PInteger): Integer;
  var
    Handle: HMODULE;
    func: _wait;
  begin
    Result := -1;
    Handle := LoadLibrary('libc.dylib');
    if Handle <> 0 then begin
      try
        @func := GetProcAddress(Handle, _PU + 'wait');
        if @func <> nil then
          Result := func(__stat_loc);
      finally
        FreeLibrary(Handle);
      end;
    end;
  end;

const
  BufferSize = 1;
var
  Output: PIOFile;
  Buffer: PAnsiChar;
  TempString, Line: AnsiString;
  BytesRead: Integer;
begin
  TempString := '';
  Output := popen(PAnsiChar(AnsiString(Command)), 'r');
  GetMem(Buffer, BufferSize);
  if Assigned(Output) then
    try
      while feof(Output) = 0 do begin
        BytesRead := fread(Buffer, 1, BufferSize, Output);
        SetLength(TempString, Length(TempString) + BytesRead);
        Move(Buffer^, TempString[Length(TempString) - (BytesRead - 1)], BytesRead);

        while Pos(#10, TempString) > 0 do begin
          Line := Copy(TempString, 1, Pos(#10, TempString) - 1);

          TThread.Synchronize(nil, procedure begin
            Callback(UTF8ToString(Line));
          end);

          TempString := Copy(TempString, Pos(#10, TempString) + 1, Length(TempString));

        end;
      end;
    finally
      pclose(Output);
      wait(nil);
      FreeMem(Buffer, BufferSize)
    end;
{$ENDIF}
end;

end.
