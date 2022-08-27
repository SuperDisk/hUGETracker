unit FURImport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ZStream, Song, Instruments, hUGEDataTypes, Constants, fgl;

function LoadSongFromFURStream(Stream: TStream): TSong;

type

  { EFURException }

  EFURException = class(Exception);

implementation

type

  { TFURStream }

  TFURStream = class(Tdecompressionstream)
    function ReadSingle: Single;
    function ReadFURString: String;
  end;

  TFURHeader = record
    FurnaceModuleFormatMagic: array[0..15] of Char;
    FormatVersion: UInt16;
    Reserved1: UInt16;
    SongInfoPointer: UInt32;
    Reserved2: UInt64;
  end;

  // Jeezus, and I thought the VGM header was big.

  TFURSongInfo = record
    InfoBlockId: UInt32; // "Info" Block Id
    SizeOfThisBlock: UInt32; // Size Of This Block
    TimeBaseOfFirstSong: UInt8; // Time Base (Of First Song)
    Speed1OfFirstSong: UInt8; // Speed 1 (Of First Song)
    Speed2OfFirstSong: UInt8; // Speed 2 (Of First Song)
    InitialArpeggioTimeOfFirstSong: UInt8; // Initial Arpeggio Time (Of First Song)
    TicksPerSecondOfFirstSong: Single; // Ticks Per Second (Of First Song)
    PatternLengthOfFirstSong: UInt16; // Pattern Length (Of First Song)
    OrdersLengthOfFirstSong: UInt16; // Orders Length (Of First Song)
    HighlightAOfFirstSong: UInt8; // Highlight A (Of First Song)
    HighlightBOfFirstSong: UInt8; // Highlight B (Of First Song)
    InstrumentCount: UInt16; // Instrument Count
    WavetableCount: UInt16; // Wavetable Count
    SampleCount: UInt16; // Sample Count
    PatternCountGlobal: UInt32; // Pattern Count (Global)
    ListOfSoundChips: array[0..31] of Byte; // List Of Sound Chips
    SoundChipVolumes: array[0..31] of Byte; // Sound Chip Volumes
    SoundChipPanning: array[0..31] of Byte; // Sound Chip Panning
    SoundChipParameters: array[0..127] of Byte; // Sound Chip Parameters
    SongName: String; // Song Name
    SongAuthor: String; // Song Author
    A4Tuning: Single; // A-4 Tuning
    LimitSlides: UInt8; // Limit Slides (>=36) Or Reserved
    LinearPitch: UInt8; // Linear Pitch (>=36) Or Reserved
    LoopModality: UInt8; // Loop Modality (>=36) Or Reserved
    ProperNoiseLayout: UInt8; // Proper Noise Layout (>=42) Or Reserved
    WaveDutyIsVolume: UInt8; // Wave Duty Is Volume (>=42) Or Reserved
    ResetMacroOnPorta: UInt8; // Reset Macro On Porta (>=45) Or Reserved
    LegacyVolumeSlides: UInt8; // Legacy Volume Slides (>=45) Or Reserved
    CompatibleArpeggio: UInt8; // Compatible Arpeggio (>=45) Or Reserved
    NoteOffResetsSlides: UInt8; // Note Off Resets Slides (>=45) Or Reserved
    TargetResetsSlides: UInt8; // Target Resets Slides (>=45) Or Reserved
    ArpeggioInhibitsPortamento: UInt8; // Arpeggio Inhibits Portamento (>=47) Or Reserved
    WackAlgorithmMacro: UInt8; // Wack Algorithm Macro (>=47) Or Reserved
    BrokenShortcutSlides: UInt8; // Broken Shortcut Slides (>=49) Or Reserved
    IgnoreDuplicateSlides: UInt8; // Ignore Duplicate Slides (>=50) Or Reserved
    StopPortamentoOnNoteOff: UInt8; // Stop Portamento On Note Off (>=62) Or Reserved
    ContinuousVibrato: UInt8; // Continuous Vibrato (>=62) Or Reserved
    BrokenDacMode: UInt8; // Broken Dac Mode (>=64) Or Reserved
    OneTickCut: UInt8; // One Tick Cut (>=65) Or Reserved
    InstrumentChangeAllowedDuringPorta: UInt8; // Instrument Change Allowed During Porta (>=66) Or Reserved
    ResetNoteBaseOnArpeggioEffectStop0000: UInt8; // Reset Note Base On Arpeggio Effect Stop (0000) (>=69) Or Reserved
    PointersToInstruments: array of UInt32; // Pointers To Instruments
    PointersToWavetables: array of UInt32; // Pointers To Wavetables
    PointersToSamples: array of UInt32; // Pointers To Samples
    PointersToPatterns: array of UInt32; // Pointers To Patterns
    OrdersOfFirstSong: array of array of UInt8; // Orders (Of First Song)
    EffectColumnsOfFirstSong: array of UInt8; // Effect Columns (Of First Song)
    ChannelHideStatusOfFirstSong: array of UInt8; // Channel Hide Status (Of First Song)
    ChannelCollapseStatusOfFirstSong: array of UInt8; // Channel Collapse Status (Of First Song)
    ChannelNamesOfFirstSong: array of String; // Channel Names (Of First Song)
    ChannelShortNamesOfFirstSong: array of String; // Channel Short Names (Of First Song)
    SongComment: String; // Song Comment
    MasterVolume10F100: Single; // Master Volume, 1.0F=100% (>=59)
    BrokenSpeedSelection: UInt8; // Broken Speed Selection
    NoSlidesOnFirstTick: UInt8; // No Slides On First Tick (>=71) Or Reserved
    NextRowResetArpPos: UInt8; // Next Row Reset Arp Pos (>=71) Or Reserved
    IgnoreJumpAtEnd: UInt8; // Ignore Jump At End (>=71) Or Reserved
    BuggyPortamentoAfterSlide: UInt8; // Buggy Portamento After Slide (>=72) Or Reserved
    NewInsAffectsEnvelopeGameBoy: UInt8; // New Ins Affects Envelope (Game Boy) (>=72) Or Reserved
    ExtchChannelStateIsShared: UInt8; // Extch Channel State Is Shared (>=78) Or Reserved
    IgnoreDacModeChangeOutsideOfIntendedChannel: UInt8; // Ignore Dac Mode Change Outside Of Intended Channel (>=83) Or Reserved
    E1XyAndE2XyAlsoTakePriorityOverSlide00: UInt8; // E1Xy And E2Xy Also Take Priority Over Slide00 (>=83) Or Reserved
    NewSegaPcmWithMacrosAndProperVolPan: UInt8; // New Sega Pcm (With Macros And Proper Vol/Pan) (>=84) Or Reserved
    WeirdFNumBlockBasedChipPitchSlides: UInt8; // Weird F-Num/Block-Based Chip Pitch Slides (>=85) Or Reserved
    SnDutyMacroAlwaysResetsPhase: UInt8; // Sn Duty Macro Always Resets Phase (>=86) Or Reserved
    PitchMacroIsLinear: UInt8; // Pitch Macro Is Linear (>=90) Or Reserved
    PitchSlideSpeedInFullLinearPitchMode: UInt8; // Pitch Slide Speed In Full Linear Pitch Mode (>=94) Or Reserved
    OldOctaveBoundaryBehavior: UInt8; // Old Octave Boundary Behavior (>=97) Or Reserved
    DisableOpn2DacVolumeControl: UInt8; // Disable Opn2 Dac Volume Control (>=98) Or Reserved
    NewVolumeScalingStrategy: UInt8; // New Volume Scaling Strategy (>=99) Or Reserved
    VolumeMacroStillAppliesAfterEnd: UInt8; // Volume Macro Still Applies After End (>=99) Or Reserved
    BrokenOutvol: UInt8; // Broken Outvol (>=99) Or Reserved
    E1XyAndE2XyStopOnSameNote: UInt8; // E1Xy And E2Xy Stop On Same Note (>=100) Or Reserved
    BrokenInitialPositionOfPortaAfterArp: UInt8; // Broken Initial Position Of Porta After Arp (>=101) Or Reserved
    SnPeriodsUnder8AreTreatedAs1: UInt8; // Sn Periods Under 8 Are Treated As 1 (>=108) Or Reserved
    Reserved: array[0..5] of Byte; // Reserved
    VirtualTempoNumeratorOfFirstSong: UInt16; // Virtual Tempo Numerator Of First Song (>=96) Or Reserved
    VirtualTempoDenominatorOfFirstSong: UInt16; // Virtual Tempo Denominator Of First Song (>=96) Or Reserved
    FirstSubsongName: String; // First Subsong Name
    FirstSubsongComment: String; // First Subsong Comment
    NumberOfAdditionalSubsongs: UInt8; // Number Of Additional Subsongs
    Reserved2: array[0..2] of Byte; // Reserved
    PointersToSubsongData: array of UInt32; // Pointers To Subsong Data
    SystemName: String; // System Name
    AlbumCategoryGameName: String; // Album/Category/Game Name
    SongNameJapanese: String; // Song Name (Japanese)
    SongAuthorJapanese: String; // Song Author (Japanese)
    SystemNameJapanese: String; // System Name (Japanese)
    AlbumCategoryGameNameJapanese: String; // Album/Category/Game Name (Japanese)
  end;

  TFURRow = record
    Note: Smallint;
    Octave: Smallint;
    Instrument: Smallint;
    Volume: Smallint;
    Effects: array of record
      EffectCode, EffectParams: Smallint;
    end;
  end;

  TFURPattern = record
    PatrBlockId: UInt32; // "Patr" Block Id
    SizeOfThisBlock: UInt32; // Size Of This Block
    Channel: UInt16; // Channel
    PatternIndex: UInt16; // Pattern Index
    Subsong: UInt16; // Subsong (>=95) Or Reserved
    Reserved: UInt16; // Reserved
    PatternData: array of TFURRow; // Pattern Data
    PatternName: String; // Pattern Name (>=51)
  end;

  TFURInstrumentMap = specialize TFPGMap<Integer, Integer>;

const
  ContinuousEffects = [
    $0
  ];

procedure CleanupPattern(Pat: PPattern);// InstMap: TTBMInstrumentMap);
var
  I: Integer;
  CurNote: Integer = NO_NOTE;
  ContinuousCode: Byte = 0;
  ContinuousParam: Byte = 0;
begin
  for I := Low(TPattern) to High(TPattern) do begin
    {if not InstMap.TryGetData(Pat^[I].Instrument-1, Pat^[I].Instrument) then
      Pat^[I].Instrument := 0;}

    if Pat^[I].Note <> NO_NOTE then
      CurNote := Pat^[I].Note;

    if (Pat^[I].Instrument <> 0) and (Pat^[I].Note = NO_NOTE) then
      Pat^[I].Note := CurNote;

    if Pat^[I].Volume = 1 then begin
      ContinuousCode := Pat^[I].EffectCode;
      ContinuousParam := Pat^[I].EffectParams.Value;
    end;

    if (ContinuousParam <> 0) and (Pat^[I].EffectCode = 0) and (Pat^[I].EffectParams.Value = 0) then begin
      Pat^[I].EffectCode := ContinuousCode;
      Pat^[I].EffectParams.Value := ContinuousParam;
    end;

    Pat^[I].Volume := 0;
  end;
end;

function ReadFURPattern(S: TFURStream; PatLength: Integer; NumFX: array of Byte): TFURPattern;
var
  I, J: Integer;
begin
  Result.PatrBlockId := S.ReadDWord;
  Result.SizeOfThisBlock := S.ReadDWord;
  Result.Channel := S.ReadWord;
  Result.PatternIndex := S.ReadWord;
  Result.Subsong := S.ReadWord;
  Result.Reserved := S.ReadWord;

  SetLength(Result.PatternData, PatLength);
  for I := Low(Result.PatternData) to High(Result.PatternData) do begin
    with Result.PatternData[I] do begin
      Note := Smallint(S.ReadWord);
      Octave := Smallint(S.ReadWord);
      Instrument := Smallint(S.ReadWord);
      Volume := Smallint(S.ReadWord);

      SetLength(Effects, NumFX[Result.Channel]);
      for J := Low(Effects) to High(Effects) do begin
        Effects[J].EffectCode := Smallint(S.ReadWord);
        Effects[J].EffectParams := Smallint(S.ReadWord);
      end;
    end;
  end;

  Result.PatternName := S.ReadFURString;
end;

function ReadFURHeader(S: TFURStream): TFURHeader;
begin
  S.ReadBuffer(Result.FurnaceModuleFormatMagic[0], SizeOf(Byte)*16);
  Result.FormatVersion := S.ReadWord;
  Result.Reserved1 := S.ReadWord;
  Result.SongInfoPointer := S.ReadDWord;
  Result.Reserved2 := S.ReadQWord;
end;

function ReadFURSongInfo(S: TFURStream): TFURSongInfo;
var
  I: Integer;
begin
  Result.InfoBlockId := S.ReadDWord;
  Result.SizeOfThisBlock := S.ReadDWord;
  Result.TimeBaseOfFirstSong := S.ReadByte;
  Result.Speed1OfFirstSong := S.ReadByte;
  Result.Speed2OfFirstSong := S.ReadByte;
  Result.InitialArpeggioTimeOfFirstSong := S.ReadByte;
  Result.TicksPerSecondOfFirstSong := S.ReadSingle;
  Result.PatternLengthOfFirstSong := S.ReadWord;
  Result.OrdersLengthOfFirstSong := S.ReadWord;
  Result.HighlightAOfFirstSong := S.ReadByte;
  Result.HighlightBOfFirstSong := S.ReadByte;
  Result.InstrumentCount := S.ReadWord;
  Result.WavetableCount := S.ReadWord;
  Result.SampleCount := S.ReadWord;
  Result.PatternCountGlobal := S.ReadDWord;
  S.ReadBuffer(Result.ListOfSoundChips[0], SizeOf(Byte)*32);
  S.ReadBuffer(Result.SoundChipVolumes[0], SizeOf(Byte)*32);
  S.ReadBuffer(Result.SoundChipPanning[0], SizeOf(Byte)*32);
  S.ReadBuffer(Result.SoundChipParameters[0], SizeOf(Byte)*128);
  Result.SongName := S.ReadFURString;
  Result.SongAuthor := S.ReadFURString;
  Result.A4Tuning := S.ReadSingle;
  Result.LimitSlides := S.ReadByte;
  Result.LinearPitch := S.ReadByte;
  Result.LoopModality := S.ReadByte;
  Result.ProperNoiseLayout := S.ReadByte;
  Result.WaveDutyIsVolume := S.ReadByte;
  Result.ResetMacroOnPorta := S.ReadByte;
  Result.LegacyVolumeSlides := S.ReadByte;
  Result.CompatibleArpeggio := S.ReadByte;
  Result.NoteOffResetsSlides := S.ReadByte;
  Result.TargetResetsSlides := S.ReadByte;
  Result.ArpeggioInhibitsPortamento := S.ReadByte;
  Result.WackAlgorithmMacro := S.ReadByte;
  Result.BrokenShortcutSlides := S.ReadByte;
  Result.IgnoreDuplicateSlides := S.ReadByte;
  Result.StopPortamentoOnNoteOff := S.ReadByte;
  Result.ContinuousVibrato := S.ReadByte;
  Result.BrokenDacMode := S.ReadByte;
  Result.OneTickCut := S.ReadByte;
  Result.InstrumentChangeAllowedDuringPorta := S.ReadByte;
  Result.ResetNoteBaseOnArpeggioEffectStop0000 := S.ReadByte;
  SetLength(Result.PointersToInstruments, Result.InstrumentCount);
  if Length(Result.PointersToInstruments) <> 0 then
    S.ReadBuffer(Result.PointersToInstruments[0], SizeOf(UInt32)*Result.InstrumentCount);
  SetLength(Result.PointersToWavetables, Result.WavetableCount);
  if Length(Result.PointersToWavetables) <> 0 then
    S.ReadBuffer(Result.PointersToWavetables[0], SizeOf(UInt32)*Result.WavetableCount);
  SetLength(Result.PointersToSamples, Result.SampleCount);
  if Length(Result.PointersToSamples) <> 0 then
    S.ReadBuffer(Result.PointersToSamples[0], SizeOf(UInt32)*Result.SampleCount);
  SetLength(Result.PointersToPatterns, Result.PatternCountGlobal);
  if Length(Result.PointersToPatterns) <> 0 then
    S.ReadBuffer(Result.PointersToPatterns[0], SizeOf(UInt32)*Result.PatternCountGlobal);


  SetLength(Result.OrdersOfFirstSong, 4);
  for I := Low(Result.OrdersOfFirstSong) to High(Result.OrdersOfFirstSong) do begin
    SetLength(Result.OrdersOfFirstSong[I], Result.OrdersLengthOfFirstSong);
    S.ReadBuffer(Result.OrdersOfFirstSong[I, 0], SizeOf(UInt8)*Result.OrdersLengthOfFirstSong);
  end;

  SetLength(Result.EffectColumnsOfFirstSong, 4); // TODO
  if Length(Result.EffectColumnsOfFirstSong) <> 0 then
    S.ReadBuffer(Result.EffectColumnsOfFirstSong[0], SizeOf(UInt8)*4);
  SetLength(Result.ChannelHideStatusOfFirstSong, 4);
  if Length(Result.ChannelHideStatusOfFirstSong) <> 0 then
    S.ReadBuffer(Result.ChannelHideStatusOfFirstSong[0], SizeOf(UInt8)*4);
  SetLength(Result.ChannelCollapseStatusOfFirstSong, 4);
  if Length(Result.ChannelCollapseStatusOfFirstSong) <> 0 then
    S.ReadBuffer(Result.ChannelCollapseStatusOfFirstSong[0], SizeOf(UInt8)*4);
  SetLength(Result.ChannelNamesOfFirstSong, 4);
  for I := Low(Result.ChannelNamesOfFirstSong) to High(Result.ChannelNamesOfFirstSong) do
    Result.ChannelNamesOfFirstSong[I] := S.ReadFURString;
  SetLength(Result.ChannelShortNamesOfFirstSong, 4);
  for I := Low(Result.ChannelShortNamesOfFirstSong) to High(Result.ChannelShortNamesOfFirstSong) do
    Result.ChannelShortNamesOfFirstSong[I] := S.ReadFURString;
  Result.SongComment := S.ReadFURString;
  Result.MasterVolume10F100 := S.ReadSingle;
  Result.BrokenSpeedSelection := S.ReadByte;
  Result.NoSlidesOnFirstTick := S.ReadByte;
  Result.NextRowResetArpPos := S.ReadByte;
  Result.IgnoreJumpAtEnd := S.ReadByte;
  Result.BuggyPortamentoAfterSlide := S.ReadByte;
  Result.NewInsAffectsEnvelopeGameBoy := S.ReadByte;
  Result.ExtchChannelStateIsShared := S.ReadByte;
  Result.IgnoreDacModeChangeOutsideOfIntendedChannel := S.ReadByte;
  Result.E1XyAndE2XyAlsoTakePriorityOverSlide00 := S.ReadByte;
  Result.NewSegaPcmWithMacrosAndProperVolPan := S.ReadByte;
  Result.WeirdFNumBlockBasedChipPitchSlides := S.ReadByte;
  Result.SnDutyMacroAlwaysResetsPhase := S.ReadByte;
  Result.PitchMacroIsLinear := S.ReadByte;
  Result.PitchSlideSpeedInFullLinearPitchMode := S.ReadByte;
  Result.OldOctaveBoundaryBehavior := S.ReadByte;
  Result.DisableOpn2DacVolumeControl := S.ReadByte;
  Result.NewVolumeScalingStrategy := S.ReadByte;
  Result.VolumeMacroStillAppliesAfterEnd := S.ReadByte;
  Result.BrokenOutvol := S.ReadByte;
  Result.E1XyAndE2XyStopOnSameNote := S.ReadByte;
  Result.BrokenInitialPositionOfPortaAfterArp := S.ReadByte;
  Result.SnPeriodsUnder8AreTreatedAs1 := S.ReadByte;
  S.ReadBuffer(Result.Reserved[0], SizeOf(Byte)*6);
  Result.VirtualTempoNumeratorOfFirstSong := S.ReadWord;
  Result.VirtualTempoDenominatorOfFirstSong := S.ReadWord;
  Result.FirstSubsongName := S.ReadFURString;
  Result.FirstSubsongComment := S.ReadFURString;
  Result.NumberOfAdditionalSubsongs := S.ReadByte;
  S.ReadBuffer(Result.Reserved[0], SizeOf(Byte)*3);
  SetLength(Result.PointersToSubsongData, Result.NumberOfAdditionalSubsongs);
  if Length(Result.PointersToSubsongData) <> 0 then
    S.ReadBuffer(Result.PointersToSubsongData[0], SizeOf(UInt32)*Result.NumberOfAdditionalSubsongs);
  Result.SystemName := S.ReadFURString;
  Result.AlbumCategoryGameName := S.ReadFURString;
  Result.SongNameJapanese := S.ReadFURString;
  Result.SongAuthorJapanese := S.ReadFURString;
  Result.SystemNameJapanese := S.ReadFURString;
  Result.AlbumCategoryGameNameJapanese := S.ReadFURString;
end;

procedure TranslateEffect(EffectCode, EffectParams: Byte; out OCode: Integer; out OParams: Byte);
begin
  OParams := EffectParams;

  case EffectCode of
    $0, $1, $2, $3, $4, $A, $B, $D, $F: begin
      OCode := EffectCode;
      OParams := EffectParams;
    end;

    $ED: begin
      OCode := $7;
      OParams := EffectParams;
    end

    else begin
      OCode := $0;
      OParams := $00;
    end;
  end;
end;

procedure TranslatePattern(const Pat: TFURPattern; PPat: PPattern);
var
  I, J: Integer;
  FRow: TFURRow;
begin
  for I := Low(Pat.PatternData) to High(Pat.PatternData) do begin
    FRow := Pat.PatternData[I];
    with PPat^[I] do begin
      if FRow.Note = 100 then begin // Note cut
        EffectCode := $E;
        EffectParams.Value := $00;
      end else if FRow.Note <> 0 then
        Note := FRow.Note + ((FRow.Octave-2) * 12);

      if FRow.Instrument <> -1 then
        Instrument := FRow.Instrument + 1;

      for J := Low(FRow.Effects) to High(FRow.Effects) do
        if (FRow.Effects[J].EffectCode <> -1) and (FRow.Effects[J].EffectParams <> -1) then begin
          TranslateEffect(
            FRow.Effects[J].EffectCode,
            FRow.Effects[J].EffectParams,
            EffectCode,
            EffectParams.Value
          );
          {if EffectCode in ContinuousEffects then
            Volume := 1; // Mark as continuous}
        end;

      if (EffectCode = 0) and (EffectParams.Value = 0) and (FRow.Volume <> -1) then begin
        EffectCode := $C;
        EffectParams.Value := FRow.Volume;
      end;
    end;
  end;
end;

function LoadSongFromFURStream(Stream: TStream): TSong;
var
  S: TFURStream;
  Header: TFURHeader;
  SongInfo: TFURSongInfo;
  Pat: TFURPattern;
  PPatHT: PPattern;
  I, J: Integer;
begin
  InitializeSong(Result);
  S := TFURStream.create(Stream);

  Header := ReadFURHeader(S);

  S.Seek(Header.SongInfoPointer, soBeginning);
  SongInfo := ReadFURSongInfo(S);

  // Copy order table over
  for I := Low(Result.OrderMatrix) to High(Result.OrderMatrix) do begin
    SetLength(Result.OrderMatrix[I], SongInfo.OrdersLengthOfFirstSong+1); // off-by-one error on my part
    for J := 0 to SongInfo.OrdersLengthOfFirstSong-1 do
      Result.OrderMatrix[I, J] := ((I+1)*100) + SongInfo.OrdersOfFirstSong[I, J];
  end;

  // Translate patterns
  for I in SongInfo.PointersToPatterns do begin
    S.Seek(I, soBeginning);
    Pat := ReadFURPattern(S, SongInfo.PatternLengthOfFirstSong, SongInfo.EffectColumnsOfFirstSong);
    PPatHT := Result.Patterns.GetOrCreateNew(((Pat.Channel+1)*100)+Pat.PatternIndex);
    TranslatePattern(Pat, PPatHT);
    CleanupPattern(PPatHT);
  end;
end;

{ TFURStream }

function TFURStream.ReadSingle: Single;
begin
  ReadBuffer(Result, SizeOf(Single));
end;

function TFURStream.ReadFURString: String;
var
  B: Byte;
begin
  Result := '';
  B := ReadByte;
  while B <> 0 do begin
    Result += Chr(B);
    B := ReadByte;
  end;
end;

end.

