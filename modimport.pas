unit MODImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Song, hugedatatypes, fgl, math, constants,
  instruments;

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

  TMODRow = record
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

const
  // https://github.com/AntonioND/gbt-player/blob/master/rgbds_example/gbt_player_bank1.asm
  GBT_WAVEFORMS: array[0..7] of array[0..15] of Byte =
    (($A5,$D7,$C9,$E1,$BC,$9A,$76,$31,$0C,$BA,$DE,$60,$1B,$CA,$03,$93),
    ($F0,$E1,$D2,$C3,$B4,$A5,$96,$87,$78,$69,$5A,$4B,$3C,$2D,$1E,$0F),
    ($FD,$EC,$DB,$CA,$B9,$A8,$97,$86,$79,$68,$57,$46,$35,$24,$13,$02),
    ($DE,$FE,$DC,$BA,$9A,$A9,$87,$77,$88,$87,$65,$56,$54,$32,$10,$12),
    ($AB,$CD,$EF,$ED,$CB,$A0,$12,$3E,$DC,$BA,$BC,$DE,$FE,$DC,$32,$10),
    ($FF,$EE,$DD,$CC,$BB,$AA,$99,$88,$77,$66,$55,$44,$33,$22,$11,$00),
    ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00),
    ($79,$BC,$DE,$EF,$FF,$EE,$DC,$B9,$75,$43,$21,$10,$00,$11,$23,$45));

var
  PeriodToCodeMap: TPeriodToCodeMap;

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
    Result := PeriodToCodeMap.KeyData[Note];
end;

function ConvertInstrument(Instr: Integer): Integer;
begin
  Result := EnsureRange(Instr, 0, 15);
end;

procedure ConvertEffect(Code, Params: Byte; out OutCode: Integer; out OutParams: TEffectParams);
var
  EP: TEffectParams absolute Params;
label IdentityEffect;
begin
  if Code in [$F, $B, $D] then begin
    OutCode := Code;
    OutParams.Value := Params + 1;
  end
  else
    case Code of
      $9: begin
        OutCode := $C;
        OutParams.Param1 := Lo(Params);
        OutParams.Param2 := Hi(Params);
      end;
      $C: begin
        if Params <> 0 then begin
          OutCode := $C;
          OutParams.Value := Trunc((Params / $40)*$F);
        end else begin
          OutCode := $E;
          OutParams.Value := 0;
        end;
      end;
      $E: begin
        if EP.Param1 = $C then begin
          OutCode := $E;
          OutParams.Value := EP.Param2;
        end
        else goto IdentityEffect;
      end;
      else begin
        IdentityEffect:
        OutCode := Code;
        OutParams.Value := Params;
      end;
    end
end;

function ConvertCell(MC: TMODRow): TCell;
begin
  Result.Note := ConvertNote(MC.Note);
  Result.Instrument := ConvertInstrument(MC.Instrument);
  ConvertEffect(MC.Effect.Code, MC.Effect.Params, Result.EffectCode, Result.EffectParams);
end;

procedure TranscribeColumn(MP: TMODPattern; Pat: PPattern; Column: Integer);
var
  I: Integer;
  LastPlayedNote: Integer;
  LastPlayedInstrument: Integer;
begin
  LastPlayedNote := C_5;
  LastPlayedInstrument := 0;

  for I := Low(MP) to High(MP) do begin
    Pat^[I] := ConvertCell(MP[I, Column]);

    if (Pat^[I].EffectCode = $C) then begin
      if (Pat^[I].Instrument = 0) then
        Pat^[I].Instrument := LastPlayedInstrument;

      if (Pat^[I].Note = NO_NOTE) then
        Pat^[I].Note := LastPlayedNote;
    end;

    if (Pat^[I].Note <> NO_NOTE) then LastPlayedNote := Pat^[I].Note;
    if (Pat^[I].Instrument <> 0) then LastPlayedInstrument := Pat^[I].Instrument;
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

  // Create the instruments.

  // First four are just the default instrument with a different duty cycle.
  Result.Instruments.Duty[1].Duty := 1;
  Result.Instruments.Duty[2].Duty := 2;
  Result.Instruments.Duty[3].Duty := 3;
  Result.Instruments.Duty[4].Duty := 0;

  for I := 8 to 15 do
    with Result.Instruments.Wave[I] do begin
      Waveform := I - 8;
      Type_ := itWave;
    end;

  // Waveforms.
  for I := Low(GBT_WAVEFORMS) to High(GBT_WAVEFORMS) do
    for J := Low(GBT_WAVEFORMS[I]) to High(GBT_WAVEFORMS[I]) do begin
      Result.Waves[I, J*2] := hi(GBT_WAVEFORMS[I, J]);
      Result.Waves[I, (J*2) + 1] := lo(GBT_WAVEFORMS[I, J]);
    end;

  // Convert all patterns
  for I := Low(ModFile.Patterns) to High(ModFile.Patterns) do begin
    TranscribeColumn(ModFile.Patterns[I], Result.Patterns.CreateNewPattern(I*10 + 0), 1);
    TranscribeColumn(ModFile.Patterns[I], Result.Patterns.CreateNewPattern(I*10 + 1), 2);
    TranscribeColumn(ModFile.Patterns[I], Result.Patterns.CreateNewPattern(I*10 + 2), 3);
    TranscribeColumn(ModFile.Patterns[I], Result.Patterns.CreateNewPattern(I*10 + 3), 4);
  end;

  // Import the order table. Uses a weird numbering scheme because hUGE has
  // 4 separate patterns like AHX and unlike MOD.
  for I := Low(Result.OrderMatrix) to High(Result.OrderMatrix) do begin
    SetLength(Result.OrderMatrix[I], ModFile.SongLen+1);

    for J := 0 to ModFile.SongLen do
      Result.OrderMatrix[I, J] := (ModFile.Positions[J]*10) + I;
  end;

  Result.Name := ModFile.Name;
end;

begin
  PeriodToCodeMap := TPeriodToCodeMap.Create;

  PeriodToCodeMap.add(1712,0);
  PeriodToCodeMap.add(1616,1);
  PeriodToCodeMap.add(1524,2);
  PeriodToCodeMap.add(1440,3);
  PeriodToCodeMap.add(1356,4);
  PeriodToCodeMap.add(1280,5);
  PeriodToCodeMap.add(1208,6);
  PeriodToCodeMap.add(1140,7);
  PeriodToCodeMap.add(1076,8);
  PeriodToCodeMap.add(1016,9);
  PeriodToCodeMap.add(960,10);
  PeriodToCodeMap.add(907,11);
  PeriodToCodeMap.add(856,12);
  PeriodToCodeMap.add(808,13);
  PeriodToCodeMap.add(762,14);
  PeriodToCodeMap.add(720,15);
  PeriodToCodeMap.add(678,16);
  PeriodToCodeMap.add(640,17);
  PeriodToCodeMap.add(604,18);
  PeriodToCodeMap.add(570,19);
  PeriodToCodeMap.add(538,20);
  PeriodToCodeMap.add(508,21);
  PeriodToCodeMap.add(480,22);
  PeriodToCodeMap.add(453,23);
  PeriodToCodeMap.add(428,24);
  PeriodToCodeMap.add(404,25);
  PeriodToCodeMap.add(381,26);
  PeriodToCodeMap.add(360,27);
  PeriodToCodeMap.add(339,28);
  PeriodToCodeMap.add(320,29);
  PeriodToCodeMap.add(302,30);
  PeriodToCodeMap.add(285,31);
  PeriodToCodeMap.add(269,32);
  PeriodToCodeMap.add(254,33);
  PeriodToCodeMap.add(240,34);
  PeriodToCodeMap.add(226,35);
  PeriodToCodeMap.add(214,36);
  PeriodToCodeMap.add(202,37);
  PeriodToCodeMap.add(190,38);
  PeriodToCodeMap.add(180,39);
  PeriodToCodeMap.add(170,40);
  PeriodToCodeMap.add(160,41);
  PeriodToCodeMap.add(151,42);
  PeriodToCodeMap.add(143,43);
  PeriodToCodeMap.add(135,44);
  PeriodToCodeMap.add(127,45);
  PeriodToCodeMap.add(120,46);
  PeriodToCodeMap.add(113,47);
  PeriodToCodeMap.add(107,48);
  PeriodToCodeMap.add(101,49);
  PeriodToCodeMap.add(95, 50);
  PeriodToCodeMap.add(90, 51);
  PeriodToCodeMap.add(85, 52);
  PeriodToCodeMap.add(80, 53);
  PeriodToCodeMap.add(75, 54);
  PeriodToCodeMap.add(71, 55);
  PeriodToCodeMap.add(67, 56);
  PeriodToCodeMap.add(63, 57);
  PeriodToCodeMap.add(60, 58);
  PeriodToCodeMap.add(56, 59);
  PeriodToCodeMap.add(53, 60);
  PeriodToCodeMap.add(50, 61);
  PeriodToCodeMap.add(47, 62);
  PeriodToCodeMap.add(45, 63);
  PeriodToCodeMap.add(42, 64);
  PeriodToCodeMap.add(40, 65);
  PeriodToCodeMap.add(37, 66);
  PeriodToCodeMap.add(35, 67);
  PeriodToCodeMap.add(33, 68);
  PeriodToCodeMap.add(31, 69);
  PeriodToCodeMap.add(30, 70);
  PeriodToCodeMap.add(28, 71);
end.

