program uge2source;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Song, Codegen;

type

  { TUGE2Source }

  TUGE2Source = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TUGE2Source }

procedure TUGE2Source.DoRun;
var
  ErrorMsg: String;
  Song: TSong;
  NonOpts: TStringArray;
  S: TStream;
  Bank: Integer;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hb:', ['help', 'bank:']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') or (ParamCount = 0) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  // Grab non-options
  NonOpts := GetNonOptions('hb:', ['help', 'bank:']);

  S := TFileStream.Create(NonOpts[0], fmOpenRead);

  try
    ReadSongFromStream(S, Song);
  finally
    S.Free;
  end;

  if HasOption('b', 'bank') then
    Bank := StrToInt(GetOptionValue('b', 'bank'))
  else
    Bank := -1;

  case LowerCase(ExtractFileExt(NonOpts[2])) of
    '.c': begin
      RenderSongToGBDKC(Song, NonOpts[1], NonOpts[2], Bank);
    end;
    '.asm': begin
      RenderSongToRGBDSAsm(Song, NonOpts[1], NonOpts[2]);
    end;
    else raise Exception.Create('Output file must be either .C or .ASM, but was ' + NonOpts[2]);
  end;

  Terminate;
end;

constructor TUGE2Source.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TUGE2Source.Destroy;
begin
  inherited Destroy;
end;

procedure TUGE2Source.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' infile.uge descriptor outfile.(asm|c)');
end;

var
  Application: TUGE2Source;
begin
  Application:=TUGE2Source.Create(nil);
  Application.Title:='uge2source';
  Application.Run;
  Application.Free;
end.

