unit TBMImport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, song, LazUTF8, HugeDatatypes, Constants, Utils;

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

  //https://github.com/stoneface86/libtrackerboy/blob/bf4993d53bc34691ca75819a96d3d00b3b699dea/src/trackerboy/data.nim#L111
  TTBMEffectType = (
    etNoEffect = 0,         // No effect, this effect column is unset.
    etPatternGoto,          // `Bxx` begin playing given pattern immediately
    etPatternHalt,          // `C00` stop playing
    etPatternSkip,          // `D00` begin playing next pattern immediately
    etSetTempo,             // `Fxx` set the tempo
    etSfx,                  // `Txx` play sound effect
    etSetEnvelope,          // `Exx` set the persistent envelope/wave id setting
    etSetTimbre,            // `Vxx` set persistent duty/wave volume setting
    etSetPanning,           // `Ixy` set channel panning setting
    etSetSweep,             // `Hxx` set the persistent sweep setting (CH1 only)
    etDelayedCut,           // `Sxx` note cut delayed by xx frames
    etDelayedNote,          // `Gxx` note trigger delayed by xx frames
    etLock,                 // `L00` (lock) stop the sound effect on the current channel
    etArpeggio,             // `0xy` arpeggio with semi tones x and y
    etPitchUp,              // `1xx` pitch slide up
    etPitchDown,            // `2xx` pitch slide down
    etAutoPortamento,       // `3xx` automatic portamento
    etVibrato,              // `4xy` vibrato
    etVibratoDelay,         // `5xx` delay vibrato xx frames on note trigger
    etTuning,               // `Pxx` fine tuning
    etNoteSlideUp,          // `Qxy` note slide up
    etNoteSlideDown,        // `Rxy` note slide down
    etSetGlobalVolume       // `Jxy` set global volume scale
  );

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
  FXType: TTBMEffectType absolute Code;
  EP: TEffectParams absolute Params;
begin
  OutCode := 0;
  OutParams.Value := 0;

  case FXType of
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
      OutParams.Param2 := Params;
    end;
    etSetTimbre: begin
      OutCode := $9;

      if Channel = itSquare then
        OutParams.Value := Params shl 6;

      if Channel = itWave then
        case Params of
          0: OutParams.Value := $00;
          1: OutParams.Value := $01;
          2: OutParams.Value := $08;
          3: OutParams.Value := $0F;
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

procedure CleanupPattern(Pat: PPattern);
var
  I: Integer;
  CurNote: Integer = NO_NOTE;
begin
  for I := Low(TPattern) to High(TPattern) do begin
    if Pat^[I].Note <> NO_NOTE then
      CurNote := Pat^[I].Note;

    if (Pat^[I].Instrument <> 0) and (Pat^[I].Note = NO_NOTE) then
      Pat^[I].Note := CurNote;
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
begin
  InitializeSong(Result);

  Stream.ReadBuffer(Header, SizeOf(TTBMHeader));

  // COMM block
  Stream.ReadBuffer(BlockHeader, SizeOf(TTBMBlockHeader));
  Stream.Seek(BlockHeader.Length, soCurrent); // Skip the comment for now

  // SONG block
  Stream.ReadBuffer(BlockHeader, SizeOf(TTBMBlockHeader));
  Result.Name := Stream.ReadLString;
  Stream.ReadBuffer(SongFormat, SizeOf(TTBMSongFormat));
  Stream.ReadByte; // ?????????????????

  Result.TicksPerRow := SongFormat.RowsPerBeat - 1;

  for I := Low(Result.OrderMatrix) to High(Result.OrderMatrix) do
    SetLength(Result.OrderMatrix[I], SongFormat.PatternCount+2); // off-by-one error on my part

  for I := 0 to SongFormat.PatternCount do begin
    Result.OrderMatrix[0, I] := 100 + Stream.ReadByte;
    Result.OrderMatrix[1, I] := 200 + Stream.ReadByte;
    Result.OrderMatrix[2, I] := 300 + Stream.ReadByte;
    Result.OrderMatrix[3, I] := 400 + Stream.ReadByte;
  end;

  for I := 0 to SongFormat.NumberOfTracks-1 do begin
    Stream.ReadBuffer(TrackFormat, SizeOf(TTBMTrackFormat));

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
    CleanupPattern(Pat);
  end;

  // INST block
  for I := 0 to Header.ICount-1 do begin
    Stream.ReadBuffer(BlockHeader, SizeOf(TTBMBlockHeader));

    InstId := Stream.ReadByte;
    InstName := Stream.ReadLString;

    Stream.ReadBuffer(InstFormat, SizeOf(TTBMInstrumentFormat));
    case InstFormat.Channel of
      0, 1: InsType := itSquare;
      2: InsType := itWave;
      3: InsType := itNoise;
    end;

    Ins := @Result.Instruments.All[UnmodInst(InsType, InstId+1)];
    Ins^.Name := InstName;

    EnvReg.ByteValue := InstFormat.Envelope;
    Ins^.InitialVolume := EnvReg.InitialVolume;
    Ins^.VolSweepDirection := TSweepType(EnvReg.Direction);
    Ins^.VolSweepAmount := EnvReg.SweepNumber;
    Ins^.Waveform := InstFormat.Envelope;

    Ins^.SubpatternEnabled := True;

    Stream.ReadBuffer(SeqFormat, SizeOf(TTBMSequenceFormat));
    for J := 0 to SeqFormat.Length-1 do begin
      Offs := ShortInt(Stream.ReadByte);
      Ins^.Subpattern[J].Note := MIDDLE_NOTE + Offs;
    end;

    Stream.ReadBuffer(SeqFormat, SizeOf(TTBMSequenceFormat));
    Stream.Seek(SeqFormat.Length, soCurrent);

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

    Stream.ReadBuffer(SeqFormat, SizeOf(TTBMSequenceFormat));
    if (InsType = itSquare) and (SeqFormat.Length > 0) then begin
      Ins^.Duty := Stream.ReadByte;
      Stream.Seek(SeqFormat.Length-1, soCurrent);
    end
    else
      Stream.Seek(SeqFormat.Length, soCurrent);
  end;

  // WAVE Block
  for I := 0 to Header.WCount-1 do begin
    Stream.ReadBuffer(BlockHeader, SizeOf(TTBMBlockHeader));
    WaveId := Stream.ReadByte;
    WaveName := Stream.ReadLString;
    Stream.ReadBuffer(FourBitWave, SizeOf(T4bitWave));
    Result.Waves[WaveId] := UnconvertWaveform(FourBitWave);
  end;

end;

end.

