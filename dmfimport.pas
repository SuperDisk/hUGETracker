unit DMFImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZStream, Song, Instruments, hUGEDataTypes, Constants;

type
  EDMFException = class(Exception);

function LoadSongFromDmfStream(Stream: TStream): TSong;

implementation

type

  { TDMFStream }

  TDMFStream = class(Tdecompressionstream)
    function ReadDMFString: String;
  end;

function LoadSongFromDmfStream(Stream: TStream): TSong;
var
  DS: TDMFStream;
  I, J, Row: Integer;
  TotalInstruments, TotalWavetables: Integer;
  Temp: QWord;
  TempS: String;
  Pat: PPattern;
  DmfNote, DmfOctave: Integer;
begin
  InitializeSong(Result);

  DS := TDMFStream.create(Stream);

  //FORMAT FLAGS
  // 16 Bytes: Format String, must be ".DelekDefleMask."
  DS.Seek(16, soCurrent);
  // 1  Byte:  File Version, must be 24 (0x18) for DefleMask v0.12.0
  Temp := DS.ReadByte;
  Writeln(temp);
  if Temp <> 24 then
    raise EDMFException.Create('This DMF file has an unsupported version.');

  //SYSTEM SET
  //1  Byte:  System:
  //SYSTEM_GAMEBOY 0x04			(SYSTEM_TOTAL_CHANNELS 4)
  if DS.ReadByte <> $04 then
    raise EDMFException.Create('This DMF file is not a Game Boy module.');

  //VISUAL INFORMATION
  // 1 Byte:   Song Name Chars Count (0-255)
  // N Bytes:  Song Name Chars
  Result.Name := DS.ReadDMFString;
  writeln(result.name);

  // 1 Byte:	  Song Author Chars Count (0-255)
  // N Bytes:   Song Author Chars
  Result.Artist := DS.ReadDMFString;
  writeln(result.artist);

  // 1 Byte:	  Highlight A in patterns
  // 1 Byte:    Highlight B in patterns
  DS.Seek(2, soCurrent);

  //MODULE INFORMATION
  //1 Byte:	  Time Base
  DS.Seek(1, soCurrent);
  //1 Byte:   Tick Time 1
  //1 Byte:   Tick Time 2
  Result.TicksPerRow := DS.ReadByte;
  DS.Seek(1, soCurrent);
  //1 Byte:   Frames Mode (0 = PAL, 1 = NTSC)
  //1 Byte:   Using Custom HZ (If set to 1, NTSC or PAL is ignored)
  //1 Byte:   Custom HZ value 1
  //1 Byte:   Custom HZ value 2
  //1 Byte:   Custom HZ value 3
  DS.Seek(5, soCurrent);
  //4 Bytes:  TOTAL_ROWS_PER_PATTERN
  Temp := DS.ReadDWord;
  Writeln(temp);
  if Temp <> 64 then
    raise EDMFException.Create('This DMF file has '+IntToStr(Temp)+' rows per pattern, but hUGETracker needs exactly 64.');
  //1 Byte:   TOTAL_ROWS_IN_PATTERN_MATRIX
  Temp := DS.ReadByte;
  writeln('rows in pattern matrix ', temp);
  for I := Low(Result.OrderMatrix) to High(Result.OrderMatrix) do
    SetLength(Result.OrderMatrix[I], Temp);

  //PATTERN MATRIX VALUES (A matrix of SYSTEM_TOTAL_CHANNELS x TOTAL_ROWS_IN_PATTERN_MATRIX)
  for I := Low(Result.OrderMatrix) to High(Result.OrderMatrix) do
    for J := Low(Result.OrderMatrix[I]) to High(Result.OrderMatrix[I]) do
      //1 Byte: Pattern Matrix Value: (Index from SYSTEM_TOTAL_CHANNELS loop, Index from TOTAL_ROWS_IN_PATTERN_MATRIX loop)
      Result.OrderMatrix[I, J] := (100*(I+1))+DS.ReadByte;
  //1 Byte: TOTAL_INSTRUMENTS
  TotalInstruments := DS.ReadByte;
  if TotalInstruments > 15 then
    raise EDMFException.Create('DMF files must have 15 instruments or less.');

  // Read all instruments
  writeln('total instruments ', totalinstruments);
  for I := 0 to TotalInstruments-1 do begin
    //	1 Byte:   Instrument Name Chars Count (0-255)
    //	N Bytes:  Instrument Name Chars
    TempS := DS.ReadDMFString;
    Result.Instruments.Duty[I+1].Name := TempS;
    Result.Instruments.Wave[I+1].Name := TempS;
    Result.Instruments.Noise[I+1].Name := TempS;

    // TODO
    Result.Instruments.Wave[I+1].Waveform := 0;

    // 1 Byte:   Instrument Mode (0 = STANDARD INS, 1 = FM INS)
    if DS.ReadByte <> 0 then
      raise EDMFException.Create('This DMF file contains an FM-type instrument, which is unsupported.');

    //ARPEGGIO MACRO
		//1 Byte: ENVELOPE_SIZE (0 - 127)
    //Repeat this ENVELOPE_SIZE times
				//4 Bytes: ENVELOPE_VALUE (signed int, offset=12)
		//IF ENVELOPE_SIZE > 0
				//1 Byte: LOOP_POSITION (-1 = NO LOOP)
		//1 Byte: ARPEGGIO MACRO MODE (0 = Normal, 1 = Fixed)
    Temp := DS.ReadByte;
    DS.Seek(4*Temp, soCurrent);
    if Temp > 0 then
      DS.Seek(1, soCurrent);
    DS.Seek(1, soCurrent);

    //DUTY/NOISE MACRO
    //1 Byte: ENVELOPE_SIZE (0 - 127)
    //Repeat this ENVELOPE_SIZE times
      //4 Bytes: ENVELOPE_VALUE
    //IF ENVELOPE_SIZE > 0
      //1 Byte: LOOP_POSITION (-1 = NO LOOP)
    Temp := DS.ReadByte;
    DS.Seek(4*Temp, soCurrent);
    if Temp > 0 then
      DS.Seek(1, soCurrent);

    //WAVETABLE MACRO
    //1 Byte: ENVELOPE_SIZE (0 - 127)
    //Repeat this ENVELOPE_SIZE times
      //4 Bytes: ENVELOPE_VALUE
    //IF ENVELOPE_SIZE > 0
      //1 Byte: LOOP_POSITION (-1 = NO LOOP)
    Temp := DS.ReadByte;
    DS.Seek(4*Temp, soCurrent);
    if Temp > 0 then
      DS.Seek(1, soCurrent);

    //1 Byte: Envelope Volume
    //1 Byte: Envelope Direction
    //1 Byte: Envelope Length
    //1 Byte: Sound Length
    with Result.Instruments.Duty[I+1] do begin
      InitialVolume := DS.ReadByte;

      VolSweepDirection := TSweepType(DS.ReadByte);
      if VolSweepDirection = stUp then
        VolSweepDirection := stDown
      else
        VolSweepDirection := stUp;

      VolSweepAmount := TEnvelopeSweepAmount(DS.ReadByte);
      //Length := DS.ReadByte;
      DS.Seek(1, soCurrent);
    end;
  end;

  //WAVETABLES DATA
  //1 Byte: TOTAL_WAVETABLES
  //Repeat this TOTAL_WAVETABLES times
  //	4 Bytes: WAVETABLE_SIZE
  //	Repeat this WAVETABLE_SIZE times
  //		4 Bytes: Wavetable Data
  TotalWavetables := DS.ReadByte;
  for I := 0 to TotalWavetables-1 do begin
    Temp := DS.ReadDWord;
    writeln('wave length was ', temp);
    for J := 0 to Temp-1 do begin
      writeln(i, ' ', j);
      Result.Waves[I, J] := Byte(DS.ReadDWord);
    end;
  end;

  for I := Low(TOrderMatrix) to High(TOrderMatrix) do begin
    Temp := DS.readbyte;
    writeln('fx columns ', temp);
    if Temp <> 1 then
      raise EDMFException.Create('This DMF file contains patterns with more than one effect column, which is unsupported.');

    writeln('iterating '+inttostr(High(Result.OrderMatrix[I]) - Low(Result.OrderMatrix[I]) + 1), ' times');
    for J := Low(Result.OrderMatrix[I]) to High(Result.OrderMatrix[I]) do begin
      Pat := Result.Patterns.GetOrCreateNew(((I+1)*100) + J);
      for Row := Low(TPattern) to High(TPattern) do begin
        with Pat^[Row] do begin
          DmfNote := DS.ReadWord;
          DmfOctave := DS.ReadWord;
          if (DmfNote = 0) and (DmfOctave = 0) then
            Note := NO_NOTE
          else
            Note := DmfNote + (12*DmfOctave) - (12*2);

          DS.Seek(2,soCurrent); //volume

          EffectCode := Byte(DS.ReadWord);
          if EffectCode = High(Byte) then
            EffectCode := 0;

          EffectParams.Value := Byte(DS.ReadWord);
          if EffectParams.Value = High(Byte) then
            EffectParams.Value := 0;

          Instrument := Byte(DS.ReadWord);
          if Instrument = High(Byte) then
            Instrument := 0
          else
            Inc(Instrument);
        end;
      end;
    end;
  end;

  DS.Free;
end;

{ TDMFStream }

function TDMFStream.ReadDMFString: String;
  Var
    TheSize : Byte;
    P : PByte ;
  begin
    ReadBuffer (TheSize,SizeOf(TheSize));
    SetLength(Result,TheSize);
    // Illegal typecast if no AnsiStrings defined.
    if TheSize>0 then
     begin
       ReadBuffer (Pointer(Result)^,TheSize);
       P:=Pointer(Result)+TheSize;
       p^:=0;
     end;
   end;


end.

