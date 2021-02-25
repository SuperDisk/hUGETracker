unit BeepBoxImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, Song, hugedatatypes, constants;

function LoadSongFromBeepBoxJsonStream(Stream: TStream): TSong;

implementation

procedure TranscribePattern(BBPat: TJSONObject; Pat: PPattern);
var
  Notes: TJSONArray;
  I: Integer;
  NoteVal, TickStart, TickEnd: Integer;
begin
  Notes := TJSONArray(BBPat.FindPath('notes'));
  for I := 0 to Notes.Count-1 do begin
    NoteVal := Notes[I].FindPath('pitches[0]').AsInteger - C_5;
    TickStart := Notes[I].FindPath('points[0].tick').AsInteger;
    TickEnd := Notes[I].FindPath('points[1].tick').AsInteger;

    with Pat^[TickStart] do begin
      Note := NoteVal;
      Instrument := 1;
      EffectCode := $0;
      EffectParams.Value := $00;
    end;

    with Pat^[TickEnd] do begin
      EffectCode := $E;
      EffectParams.Value := $00;
    end;
  end;
end;

function LoadSongFromBeepBoxJsonStream(Stream: TStream): TSong;
var
  jData: TJSONData;
  jObject: TJSONObject;
begin
  jData := GetJSON(Stream);
  jObject := TJSONObject(jData);

  InitializeSong(Result);
  Result.TicksPerRow := 6;
  TranscribePattern(TJSONObject(jObject.FindPath('channels[0].patterns[0]')), Result.Patterns.GetOrCreateNew(0));
  TranscribePattern(TJSONObject(jObject.FindPath('channels[1].patterns[0]')), Result.Patterns.GetOrCreateNew(1));
end;

end.

