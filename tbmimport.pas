unit TBMImport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, song, LazUTF8, HugeDatatypes, Constants, Utils, fgl;

type
  ETBMException = class(Exception);

function LoadSongFromTbmStream(Stream: TStream): TSong;

implementation

type
  TTBMHeader = packed record
    Signature: array[0..11] of Char; // ' TRACKERBOY '
    VersionMajor: DWord;
    VersionMinor: DWord;
    VersionPatch: DWord;
    MRev: Byte;
    NRev: Byte;
    Reserved1: Word;

    Title: array[0..31] of Char;
    Artist: array[0..31] of Char;
    Copyright: array[0..31] of Char;

    ICount: Byte;
    SCount: Byte;
    WCount: Byte;
    System: Byte;
    CustomFrameRate: Word;

    Reserved2: array[0..29] of Byte;
  end;

  TTBMBlockHeader = packed record
    Id: array[0..3] of Char;
    Length: DWord;
  end;

  TTBMSongFormat = packed record
    RowsPerBeat: Byte;
    RowsPerMeasure: Byte;
    Speed: Byte;
    PatternCount: Byte;
    RowsPerTrack: Byte;
    NumberOfTracks: Word;
  end;

  TTBMTrackFormat = packed record
    Channel: Byte;
    TrackId: Byte;
    Rows: Byte;
  end;

  TTBMEffect = packed record
    EffectType: Byte;
    Param: Byte;
  end;

  TTBMTrackRow = packed record
    Note: Byte;
    Instrument: Byte;
    Effects: array[0..2] of TTBMEffect;
  end;

  TTBMRowFormat = packed record
    RowNo: Byte;
    RowData: TTBMTrackRow;
  end;

  TTBMInstrumentFormat = packed record
    Channel: Byte;
    EnvelopeEnabled: Byte;
    Envelope: Byte;
  end;

  TTBMSequenceFormat = packed record
    Length: Word;
    LoopEnabled: Byte;
    LoopIndex: Byte;
  end;

  TStreamHelper = class helper for TStream
    function ReadLString: String;
  end;

  TTBMInstrumentMap = specialize TFPGMap<Integer, Integer>;

  //https://github.com/stoneface86/libtrackerboy/blob/bf4993d53bc34691ca75819a96d3d00b3b699dea/src/trackerboy/data.nim#L111
  TTBMEffectType = (
    etNoEffect = 0,         // No effect, this effect column is unset.
    etPatternGoto = 1,          // `Bxx` begin playing given pattern immediately
    etPatternHalt = 2,          // `C00` stop playing
    etPatternSkip = 3,          // `D00` begin playing next pattern immediately
    etSetTempo = 4,             // `Fxx` set the tempo
    etSfx = 5,                  // `Txx` play sound effect
    etSetEnvelope = 6,          // `Exx` set the persistent envelope/wave id setting
    etSetTimbre = 7,            // `Vxx` set persistent duty/wave volume setting
    etSetPanning = 8,           // `Ixy` set channel panning setting
    etSetSweep = 9,             // `Hxx` set the persistent sweep setting (CH1 only)
    etDelayedCut = 10,           // `Sxx` note cut delayed by xx frames
    etDelayedNote = 11,          // `Gxx` note trigger delayed by xx frames
    etLock = 12,                 // `L00` (lock) stop the sound effect on the current channel
    etArpeggio = 13,             // `0xy` arpeggio with semi tones x and y
    etPitchUp = 14,              // `1xx` pitch slide up
    etPitchDown = 15,            // `2xx` pitch slide down
    etAutoPortamento = 16,       // `3xx` automatic portamento
    etVibrato = 17,              // `4xy` vibrato
    etVibratoDelay = 18,         // `5xx` delay vibrato xx frames on note trigger
    etTuning = 19,               // `Pxx` fine tuning
    etNoteSlideUp = 20,          // `Qxy` note slide up
    etNoteSlideDown = 21,        // `Rxy` note slide down
    etSetGlobalVolume = 22       // `Jxy` set global volume scale
  );

const
  ContinuousEffects = [
    etArpeggio,
    etPitchUp,
    etPitchDown,
    etAutoPortamento,
    etVibrato
  ];

function TStreamHelper.ReadLString: String;
var
  Len: Word;
  Buf: array of byte;
begin
  Self.ReadBuffer(Len, SizeOf(Word));
  SetLength(Buf, Len);
  Self.ReadBuffer(Buf[0], Len);
  Result := UTF8CStringToUTF8String(@Buf[0], Len);
end;

procedure ConvertEffect(Code, Params: Byte; Channel: TInstrumentType;
  out OutCode: Integer; out OutParams: TEffectParams);
var
  EP: TEffectParams absolute Params;
begin
  OutCode := 0;
  OutParams.Value := 0;

  case TTBMEffectType(Code) of
    etNoEffect: begin
      // no op
    end;
    etPatternGoto: begin
      OutCode := $B;
      OutParams.Value := Params + 1;
    end;
    etPatternHalt: begin
      // no op
    end;
    etPatternSkip: begin
      OutCode := $D;
      OutParams.Value := Params + 1;
    end;
    etSetTempo: begin
      OutCode := $F;
      OutParams.Value := Params;
    end;
    etSfx: begin
      // no op
    end;
    etSetEnvelope: begin
      OutCode := $C;
      OutParams.Value := Params;
    end;
    etSetTimbre: begin
      OutCode := $9;

      if Channel = itSquare then
        OutParams.Value := Params shl 6;

      if Channel = itWave then begin
        OutCode := $C;

        case Params of
          0: OutParams.Value := $00;
          1: OutParams.Value := $01;
          2: OutParams.Value := $08;
          3: OutParams.Value := $0F;
        end;
      end;

      if Channel = itNoise then begin
        if Params > 0 then
          OutParams.Value := $08
        else
          OutParams.Value := $00;
      end;
    end;
    etSetPanning: begin
      // TODO
    end;
    etSetSweep: begin
      // no op
    end;
    etDelayedCut: begin
      OutCode := $E;
      OutParams.Value := Params;
    end;
    etDelayedNote: begin
      OutCode := $7;
      OutParams.Value := Params;
    end;
    etLock: begin
      // no op
    end;
    etArpeggio: begin
      OutCode := $0;
      OutParams.Value := Params;
    end;
    etPitchUp: begin
      OutCode := $1;
      OutParams.Value := Params;
    end;
    etPitchDown: begin
      OutCode := $2;
      OutParams.Value := Params;
    end;
    etAutoPortamento: begin
      OutCode := $3;
      OutParams.Value := Params;
    end;
    etVibrato: begin
      OutCode := $4;
      OutParams.Param2 := $4;
      OutParams.Param1 := Params;
    end;
    etVibratoDelay: begin
      // no op
    end;
    etTuning: begin
      OutCode := $4;
      OutParams.Param2 := $0;
      OutParams.Param1 := Abs(Params - $80);
    end;
    etNoteSlideUp: begin
      OutCode := $1;
      OutParams.Value := Params;
    end;
    etNoteSlideDown: begin
      OutCode := $2;
      OutParams.Value := Params;
    end;
    etSetGlobalVolume: begin
      OutCode := $5;
      OutParams.Value := Params;
    end;
  end;
end;

function ConverTBMRow(Row: TTBMTrackRow; RowType: TInstrumentType): TCell;
var
  I: Integer;
begin
  BlankCell(Result);

  with Result do begin
    Note := (Row.Note-1);
    if RowType = itNoise then
      Inc(Note, 4);

    Instrument := Row.Instrument;

    for I := Low(Row.Effects) to High(Row.Effects) do begin
      if (Row.Effects[I].EffectType = 0) and (Row.Effects[I].Param = 0) then
        Continue;

      ConvertEffect(Row.Effects[I].EffectType, Row.Effects[I].Param, RowType, Result.EffectCode, Result.EffectParams);
      if TTBMEffectType(Row.Effects[I].EffectType) in ContinuousEffects then
        Result.Volume := 1 // Mark this effect as "continuous"
      else
        Result.Volume := 0;
    end;

    if Row.Note = 0 then
      Note := NO_NOTE;

    if Row.Note = 85 then begin
      Note := NO_NOTE;
      EffectCode := $E;
      EffectParams.Value := $00;
    end;
  end;
end;

procedure CleanupPattern(Pat: PPattern; InstMap: TTBMInstrumentMap);
var
  I: Integer;
  CurNote: Integer = NO_NOTE;
  ContinuousCode: Byte = 0;
  ContinuousParam: Byte = 0;
begin
  for I := Low(TPattern) to High(TPattern) do begin
    if not InstMap.TryGetData(Pat^[I].Instrument-1, Pat^[I].Instrument) then
      Pat^[I].Instrument := 0;

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

function LoadSongFromTbmStream(Stream: TStream): TSong;
var
  Header: TTBMHeader;
  BlockHeader: TTBMBlockHeader;
  SongFormat: TTBMSongFormat;
  TrackFormat: TTBMTrackFormat;
  RowFormat: TTBMRowFormat;
  I, J: Integer;
  Pat: PPattern;

  InstId: Byte;
  InstName: String;
  InstFormat: TTBMInstrumentFormat;
  SeqFormat: TTBMSequenceFormat;

  InsType: TInstrumentType;
  Ins: ^TInstrument;

  EnvReg: TEnvelopeRegister;
  PitchOffset: Integer;
  Offs: ShortInt;

  WaveId: Byte;
  WaveName: String;
  FourBitWave: T4bitWave;

  SquareMap, WaveMap, NoiseMap: TTBMInstrumentMap;
  SeenSquare, SeenWave, SeenNoise: Integer;
  NewInstId: Integer;
  TracksForCleanup: array of TTBMTrackFormat;
begin
  try
    InitializeSong(Result);

    Stream.ReadBuffer(Header, SizeOf(TTBMHeader));

    if Header.SCount > 1 then
      raise ETBMException.Create('hUGETracker only supports loading TBM modules with one song.');

    // COMM block
    Stream.ReadBuffer(BlockHeader, SizeOf(TTBMBlockHeader));
    Stream.Seek(BlockHeader.Length, soCurrent); // Skip the comment for now

    // SONG block
    Stream.ReadBuffer(BlockHeader, SizeOf(TTBMBlockHeader));
    Result.Name := Stream.ReadLString;
    Stream.ReadBuffer(SongFormat, SizeOf(TTBMSongFormat));

    // Number of visible effect columns-- undocumented currently but I asked
    // stoneface about it.
    Stream.ReadByte;

    Result.TicksPerRow := SongFormat.RowsPerBeat;

    for I := Low(Result.OrderMatrix) to High(Result.OrderMatrix) do
      SetLength(Result.OrderMatrix[I], SongFormat.PatternCount+2); // off-by-one error on my part

    for I := 0 to SongFormat.PatternCount do begin
      Result.OrderMatrix[0, I] := 100 + Stream.ReadByte;
      Result.OrderMatrix[1, I] := 200 + Stream.ReadByte;
      Result.OrderMatrix[2, I] := 300 + Stream.ReadByte;
      Result.OrderMatrix[3, I] := 400 + Stream.ReadByte;
    end;

    SetLength(TracksForCleanup, SongFormat.NumberOfTracks);
    for I := 0 to SongFormat.NumberOfTracks-1 do begin
      Stream.ReadBuffer(TrackFormat, SizeOf(TTBMTrackFormat));
      TracksForCleanup[I] := TrackFormat;

      case TrackFormat.Channel of
        0, 1: InsType := itSquare;
        2: InsType := itWave;
        3: InsType := itNoise;
      end;

      Pat := Result.Patterns.GetOrCreateNew(((TrackFormat.Channel+1)*100) + TrackFormat.TrackId);
      for J := 0 to TrackFormat.Rows do begin
        Stream.ReadBuffer(RowFormat, SizeOf(TTBMRowFormat));
        Pat^[RowFormat.RowNo] := ConverTBMRow(RowFormat.RowData, InsType);
      end;
    end;

    // INST block
    SquareMap := TTBMInstrumentMap.Create;
    WaveMap := TTBMInstrumentMap.Create;
    NoiseMap := TTBMInstrumentMap.Create;
    SeenSquare := 1;
    SeenWave := 1;
    SeenNoise := 1;

    for I := 0 to Header.ICount-1 do begin
      Stream.ReadBuffer(BlockHeader, SizeOf(TTBMBlockHeader));

      InstId := Stream.ReadByte;
      InstName := Stream.ReadLString;

      Stream.ReadBuffer(InstFormat, SizeOf(TTBMInstrumentFormat));

      case InstFormat.Channel of
        0, 1: begin
          InsType := itSquare;
          SquareMap.Add(InstId, SeenSquare);
          NewInstId := SeenSquare;
          Inc(SeenSquare);
        end;
        2: begin
          InsType := itWave;
          WaveMap.Add(InstId, SeenWave);
          NewInstId := SeenWave;
          Inc(SeenWave);
        end;
        3: begin
          InsType := itNoise;
          NoiseMap.Add(InstId, SeenNoise);
          NewInstId := SeenNoise;
          Inc(SeenNoise);
        end;
      end;

      Ins := @Result.Instruments.All[UnmodInst(InsType, NewInstId)];
      Ins^.Name := InstName;

      EnvReg.ByteValue := InstFormat.Envelope;
      Ins^.InitialVolume := EnvReg.InitialVolume;
      if EnvReg.Direction then
        Ins^.VolSweepDirection := stUp
      else
        Ins^.VolSweepDirection := stDown;
      Ins^.VolSweepAmount := EnvReg.SweepNumber;
      Ins^.Waveform := InstFormat.Envelope;

      Ins^.SubpatternEnabled := True;

      // Arpeggio sequence
      Stream.ReadBuffer(SeqFormat, SizeOf(TTBMSequenceFormat));
      for J := 0 to SeqFormat.Length-1 do begin
        Offs := ShortInt(Stream.ReadByte);
        Ins^.Subpattern[J].Note := MIDDLE_NOTE + Offs;
      end;

      Ins^.Subpattern[SeqFormat.Length].Volume := SeqFormat.LoopIndex;

      // Panning sequence
      Stream.ReadBuffer(SeqFormat, SizeOf(TTBMSequenceFormat));
      Stream.Seek(SeqFormat.Length, soCurrent);

      // Pitch sequence
      Stream.ReadBuffer(SeqFormat, SizeOf(TTBMSequenceFormat));
      PitchOffset := 0;
      Offs := 0;
      for J := 0 to SeqFormat.Length-1 do begin
        Offs := ShortInt(Stream.ReadByte);

        if Offs > PitchOffset then
          Ins^.Subpattern[J].EffectCode := $1
        else if Offs < PitchOffset then
          Ins^.Subpattern[J].EffectCode := $2
        else
          Continue;

        Ins^.Subpattern[J].EffectParams.Value := Abs(PitchOffset - Offs);
        PitchOffset := Offs;
      end;

      Ins^.Subpattern[SeqFormat.Length].Volume := SeqFormat.LoopIndex;

      // Duty/noise-type sequence
      Stream.ReadBuffer(SeqFormat, SizeOf(TTBMSequenceFormat));
      case InsType of
        itSquare: begin
          for J := 0 to SeqFormat.Length-1 do begin
            Ins^.Subpattern[J].EffectCode := $9;
            case Stream.ReadByte of
              0: Ins^.Subpattern[J].EffectParams.Value := $00;
              1: Ins^.Subpattern[J].EffectParams.Value := $40;
              2: Ins^.Subpattern[J].EffectParams.Value := $80;
              3: Ins^.Subpattern[J].EffectParams.Value := $C0;
            end;
          end;
        end;
        itWave: begin
          for J := 0 to SeqFormat.Length-1 do begin
            Ins^.Subpattern[J].EffectCode := $C;
            case Stream.ReadByte of
              0: Ins^.Subpattern[J].EffectParams.Value := $00;
              1: Ins^.Subpattern[J].EffectParams.Value := $01;
              2: Ins^.Subpattern[J].EffectParams.Value := $08;
              3: Ins^.Subpattern[J].EffectParams.Value := $0F;
            end;
          end;
        end;
        itNoise: begin
          for J := 0 to SeqFormat.Length-1 do begin
            Ins^.Subpattern[J].EffectCode := $9;
            if Stream.ReadByte = 0 then
              Ins^.Subpattern[J].EffectParams.Value := $00
            else
              Ins^.Subpattern[J].EffectParams.Value := $80;
          end;
        end
      end;

      Ins^.Subpattern[SeqFormat.Length].Volume := SeqFormat.LoopIndex;
    end;

    // WAVE Block
    for I := 0 to Header.WCount-1 do begin
      Stream.ReadBuffer(BlockHeader, SizeOf(TTBMBlockHeader));
      WaveId := Stream.ReadByte;
      WaveName := Stream.ReadLString;
      Stream.ReadBuffer(FourBitWave, SizeOf(T4bitWave));
      Result.Waves[WaveId] := UnconvertWaveform(FourBitWave);
    end;

    // Cleanup patterns
    for TrackFormat in TracksForCleanup do begin
      Pat := Result.Patterns.GetOrCreateNew(((TrackFormat.Channel+1)*100) + TrackFormat.TrackId);

      case TrackFormat.Channel of
        0, 1: CleanupPattern(Pat, SquareMap);
        2: CleanupPattern(Pat, WaveMap);
        3: CleanupPattern(Pat, NoiseMap);
      end;
    end;
  finally
    if Assigned(SquareMap) then SquareMap.Free;
    if Assigned(WaveMap) then WaveMap.Free;
    if Assigned(NoiseMap) then NoiseMap.Free;
  end;
end;

end.

