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

end.

