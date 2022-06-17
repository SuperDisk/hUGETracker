unit VGM;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

var
  RecordingVGM: Boolean = False;

procedure BeginRecordingVGM(F: String);
procedure EndRecordingVGM;

procedure VGMWriteReg(Command: Integer; Param: Integer);
procedure VGMWait(Amount: Integer);

implementation

var
  VGMFile: TFileStream;

procedure BeginRecordingVGM(F: String);
begin
  VGMFile := TFileStream.Create(F, fmOpenWrite);
end;

procedure EndRecordingVGM;
begin

end;

procedure VGMWriteReg(Command: Integer; Param: Integer);
begin

end;

procedure VGMWait(Amount: Integer);
begin

end;

end.

