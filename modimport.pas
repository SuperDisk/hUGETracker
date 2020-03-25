unit MODImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Song, hugedatatypes, fgl, utils, constants;

type
  TPeriodToCodeMap = specialize TFPGMap<Integer, Integer>;

  // TODO: Find some way to de-duplicate these from the instruments file
  // without a circular reference
  Bit = Boolean;
  TwoBits = 0..%11;
  ThreeBits = 0..%111;
  FourBits = 0..%1111;
  FiveBits = 0..%11111;
  SixBits = 0..%111111;
  SevenBits = 0..%1111111;
  EightBits = Byte;
  TwelveBits = 0..%111111111111;

  { TMODSample }

  TMODSample = packed record
    Name: array[0..21] of char; //string[21];
    SampleLength: Word;
    Finetune: Byte;
    Volume: Byte;
    RepeatPoint: Word;
    RepeatLength: Word;
  end;

  { TMODRawRow }

  TMODRawRow = packed array[0..3] of Byte;

  { TMODRow }

  TMODRow = bitpacked record
    Note: Integer;
    Instrument: Integer;
    Effect: bitpacked record
      case Boolean of
        True: (Code, Params: Byte);
        False: (Value: Word);
    end
  end;

  TMODPattern = array[0..63] of array[1..4] of TMODRow;

  { TMODFile }

  TMODFile = packed record
    Name: array[0..19] of Char; //string[19];
    Samples: array[1..31] of TMODSample;
    SongLen: Byte;
    NumPatternsMagic: Byte;
    Positions: array[0..127] of Byte;
    MKMagic: array[0..3] of char; //string[3];
    Patterns: array of TMODPattern;
  end;

function LoadSongFromModStream(Stream: TStream): TSong;

var
  PeriodsToCodeMap: TPeriodToCodeMap;

implementation

function RawRowToRegularRow(RawRow: TMODRawRow): TMODRow;
begin
  Result.Note := ((RawRow[0] and %1111) shl 8) or RawRow[1];
  Result.Effect.Code := RawRow[2] and %1111;
  Result.Effect.Params := RawRow[3];
  Result.Instrument := (RawRow[0] and %11110000) or ((RawRow[2] and %11110000) shr 4);
end;

function ConvertNote(Note: Integer): Integer;
begin
  if Note = 0 then
    Result := NO_NOTE
  else
    Result := PeriodsToCodeMap.KeyData[Note];
end;

function ConvertInstrument(Instr: Integer): Integer;
begin
  if Instr = 0 then
    Result := 0
  else
    Result := 1;
end;

procedure ConvertEffect(Code, Params: Integer; out OutCode: Integer; out OutParams: TEffectParams);
begin
  case Code of
    $C: begin
      OutCode := $C;
      OutParams.Value := ;
    end;
    else begin
      OutCode := Code;
      OutParams := Params;
    end;
  end
end;

function ConvertCell(MC: TMODRow): TCell;
begin
  Result.Note := ConvertNote(MC.Note);
  Result.Instrument := ConvertInstrument(MC.Instrument);
  ConvertEffect(MC.Effect.Code, MC.Effect.Params, Result.EffectCode, Result.EffectParams);
end;

procedure TranscribePattern(MP: TMODPattern; P1, P2, P3, P4: PPattern);
var
  I: Integer;
begin
  for I := Low(MP) to High(MP) do begin
    P1^[I] := ConvertCell(MP[I, 1]);
    P2^[I] := ConvertCell(MP[I, 2]);
    P3^[I] := ConvertCell(MP[I, 3]);
    P4^[I] := ConvertCell(MP[I, 4]);
  end;
end;

function LoadSongFromModStream(Stream: TStream): TSong;
var
  ModFile: TMODFile;
  I, J, K: Integer;
  MaxOrder: Integer;
  RawRow: TMODRawRow;
begin
  Stream.Read(ModFile, SizeOf(ModFile) - SizeOf(ModFile.Patterns));
  for I := Low(ModFile.Samples) to High(ModFile.Samples) do
    with ModFile.Samples[I] do begin
      SampleLength := BEtoN(SampleLength);
      RepeatPoint := BEtoN(RepeatPoint);
      RepeatLength := BEtoN(RepeatLength);
    end;

  MaxOrder := 0;
  for I := Low(ModFile.Positions) to High(ModFile.Positions) do
    if ModFile.Positions[I] > MaxOrder then
      MaxOrder := ModFile.Positions[I];

  SetLength(ModFile.Patterns, MaxOrder+1);
  for I := 0 to MaxOrder do
    for J := Low(TMODPattern) to High(TMODPattern) do
      for K := 1 to 4 do begin
        Stream.Read(RawRow, SizeOf(TMODRawRow));
        ModFile.Patterns[I, J, K] := RawRowToRegularRow(RawRow);
      end;

  InitializeSong(Result);

  for I := Low(ModFile.Patterns) to High(ModFile.Patterns) do
    TranscribePattern(ModFile.Patterns[I],
                      Result.Patterns.CreateNewPattern(I*10 + 0),
                      Result.Patterns.CreateNewPattern(I*10 + 1),
                      Result.Patterns.CreateNewPattern(I*10 + 2),
                      Result.Patterns.CreateNewPattern(I*10 + 3));

  for I := Low(Result.OrderMatrix) to High(Result.OrderMatrix) do begin
    SetLength(Result.OrderMatrix[I], ModFile.SongLen);

    for J := 0 to ModFile.SongLen do
      Result.OrderMatrix[I, J] := (ModFile.Positions[J]*10) + I;
  end;

  Result.Name := ModFile.Name;
end;

begin
  PeriodsToCodeMap := TPeriodToCodeMap.Create;

  PeriodsToCodeMap.add(1712,0);
  PeriodsToCodeMap.add(1616,1);
  PeriodsToCodeMap.add(1524,2);
  PeriodsToCodeMap.add(1440,3);
  PeriodsToCodeMap.add(1356,4);
  PeriodsToCodeMap.add(1280,5);
  PeriodsToCodeMap.add(1208,6);
  PeriodsToCodeMap.add(1140,7);
  PeriodsToCodeMap.add(1076,8);
  PeriodsToCodeMap.add(1016,9);
  PeriodsToCodeMap.add(960,10);
  PeriodsToCodeMap.add(907,11);
  PeriodsToCodeMap.add(856,12);
  PeriodsToCodeMap.add(808,13);
  PeriodsToCodeMap.add(762,14);
  PeriodsToCodeMap.add(720,15);
  PeriodsToCodeMap.add(678,16);
  PeriodsToCodeMap.add(640,17);
  PeriodsToCodeMap.add(604,18);
  PeriodsToCodeMap.add(570,19);
  PeriodsToCodeMap.add(538,20);
  PeriodsToCodeMap.add(508,21);
  PeriodsToCodeMap.add(480,22);
  PeriodsToCodeMap.add(453,23);
  PeriodsToCodeMap.add(428,24);
  PeriodsToCodeMap.add(404,25);
  PeriodsToCodeMap.add(381,26);
  PeriodsToCodeMap.add(360,27);
  PeriodsToCodeMap.add(339,28);
  PeriodsToCodeMap.add(320,29);
  PeriodsToCodeMap.add(302,30);
  PeriodsToCodeMap.add(285,31);
  PeriodsToCodeMap.add(269,32);
  PeriodsToCodeMap.add(254,33);
  PeriodsToCodeMap.add(240,34);
  PeriodsToCodeMap.add(226,35);
  PeriodsToCodeMap.add(214,36);
  PeriodsToCodeMap.add(202,37);
  PeriodsToCodeMap.add(190,38);
  PeriodsToCodeMap.add(180,39);
  PeriodsToCodeMap.add(170,40);
  PeriodsToCodeMap.add(160,41);
  PeriodsToCodeMap.add(151,42);
  PeriodsToCodeMap.add(143,43);
  PeriodsToCodeMap.add(135,44);
  PeriodsToCodeMap.add(127,45);
  PeriodsToCodeMap.add(120,46);
  PeriodsToCodeMap.add(113,47);
  PeriodsToCodeMap.add(107,48);
  PeriodsToCodeMap.add(101,49);
  PeriodsToCodeMap.add(95, 50);
  PeriodsToCodeMap.add(90, 51);
  PeriodsToCodeMap.add(85, 52);
  PeriodsToCodeMap.add(80, 53);
  PeriodsToCodeMap.add(75, 54);
  PeriodsToCodeMap.add(71, 55);
  PeriodsToCodeMap.add(67, 56);
  PeriodsToCodeMap.add(63, 57);
  PeriodsToCodeMap.add(60, 58);
  PeriodsToCodeMap.add(56, 59);
  PeriodsToCodeMap.add(53, 60);
  PeriodsToCodeMap.add(50, 61);
  PeriodsToCodeMap.add(47, 62);
  PeriodsToCodeMap.add(45, 63);
  PeriodsToCodeMap.add(42, 64);
  PeriodsToCodeMap.add(40, 65);
  PeriodsToCodeMap.add(37, 66);
  PeriodsToCodeMap.add(35, 67);
  PeriodsToCodeMap.add(33, 68);
  PeriodsToCodeMap.add(31, 69);
  PeriodsToCodeMap.add(30, 70);
  PeriodsToCodeMap.add(28, 71);
end.

