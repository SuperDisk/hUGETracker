unit TBMImport;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, song, LazUTF8, HugeDatatypes, Constants, Utils;

function LoadSongFromTbmStream(Stream: TStream): TSong;

implementation

const
  TBM_EFFECTS: array[0..22] of Byte = (
    $0,  // No effect, this effect column is unset.
    $B,  // `Bxx` begin playing given pattern immediately
    $0,  // `C00` stop playing
    $D,  // `D00` begin playing next pattern immediately
    $F,  // `Fxx` set the tempo
    $0,  // `Txx` play sound effect
    $C,  // `Exx` set the persistent envelope/wave id setting
    $9,  // `Vxx` set persistent duty/wave volume setting
    $8,  // `Ixy` set channel panning setting
    $0,  // `Hxx` set the persistent sweep setting (CH1 only)
    $E,  // `Sxx` note cut delayed by xx frames
    $7,  // `Gxx` note trigger delayed by xx frames
    $0,  // `L00` (lock) stop the sound effect on the current channel
    $0,  // `0xy` arpeggio with semi tones x and y
    $1,  // `1xx` pitch slide up
    $2,  // `2xx` pitch slide down
    $3,  // `3xx` automatic portamento
    $4,  // `4xy` vibrato
    $0,  // `5xx` delay vibrato xx frames on note trigger
    $0,  // `Pxx` fine tuning
    $1,  // `Qxy` note slide up
    $2,  // `Rxy` note slide down
    $5   // `Jxy` set global volume scale
  );

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

function ConverTBMRow(Row: TTBMTrackRow; RowType: TInstrumentType): TCell;
begin
  with Result do begin
    Note := (Row.Note-1);
    if RowType = itNoise then
      Inc(Note, 4);
    Instrument := Row.Instrument;
    EffectCode := TBM_EFFECTS[Row.Effects[0].EffectType];
    EffectParams.Value := Row.Effects[0].Param;

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
begin
  InitializeSong(Result);

  Stream.ReadBuffer(Header, SizeOf(TTBMHeader));
  with Header do begin
    writeln('Signature = ', signature);
    writeln('Title = ', title);
    writeln('Artist = ', artist);
    writeln('Copyright = ', copyright);
    writeln('icount = ', icount);
    writeln('scount = ', scount);
    writeln('wcount = ', wcount);
  end;

  // COMM block
  Stream.ReadBuffer(BlockHeader, SizeOf(TTBMBlockHeader));
  Stream.Seek(BlockHeader.Length, soCurrent); // Skip the comment for now

  // SONG block
  Stream.ReadBuffer(BlockHeader, SizeOf(TTBMBlockHeader));
  Writeln(Stream.ReadLString); // Song name
  Stream.ReadBuffer(SongFormat, SizeOf(TTBMSongFormat));
  Writeln('Song has ', songformat.PatternCount, ' patterns ', songformat.NumberOfTracks, ' tracks');
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
    writeln(blockheader.id);

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
end;

end.

