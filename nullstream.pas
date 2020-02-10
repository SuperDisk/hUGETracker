unit NullStream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TNullStream }

  TNullStream = class(TStream)
    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

implementation

{ TNullStream }

function TNullStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result:= 0;
end;

function TNullStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result:= 0;
end;

end.

