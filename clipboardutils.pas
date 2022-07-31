unit ClipboardUtils;

{$mode delphi}

interface

uses
  Classes, SysUtils, Constants, Clipbrd, HugeDatatypes, Utils, Math;

type
  EClipboardFormatException = class(Exception);

function GetPastedCells: TSelection;
procedure CopyCells(Selection: TSelection);

function GetFamitrackerPastedCells: TSelection;

implementation

const
  // FamiTrackerTypes.h
  FTM_EFFECTS: array[0..44] of Integer = (
    $0, //EF_NONE = 0,
	  $F, //EF_SPEED,           	// Speed
	  $B, //EF_JUMP,            	// Jump
	  $D, //EF_SKIP,            	// Skip
	  $0, //EF_HALT,            	// Halt
	  $C, //EF_VOLUME,          	// Volume
	  $3, //EF_PORTAMENTO,      	// Porta on
	  $0, //EF_PORTAOFF,        	// Porta off		// unused
	  $0, //EF_SWEEPUP,         	// Sweep up
	  $0, //EF_SWEEPDOWN,       	// Sweep down
	  $0, //EF_ARPEGGIO,        	// Arpeggio
	  $4, //EF_VIBRATO,         	// Vibrato
	  $0, //EF_TREMOLO,         	// Tremolo
	  $0, //EF_PITCH,           	// Pitch
	  $0, //EF_DELAY,           	// Note delay
	  $0, //EF_DAC,             	// DAC setting
	  $1, //EF_PORTA_UP,        	// Portamento up
	  $2, //EF_PORTA_DOWN,      	// Portamento down
	  $9, //EF_DUTY_CYCLE,      	// Duty cycle
	  $0, //EF_SAMPLE_OFFSET,   	// Sample offset
	  $0, //EF_SLIDE_UP,        	// Slide up
	  $0, //EF_SLIDE_DOWN,      	// Slide down
	  $A, //EF_VOLUME_SLIDE,    	// Volume slide
	  $E, //EF_NOTE_CUT,        	// Note cut
	  $0, //EF_RETRIGGER,       	// DPCM retrigger
	  $0, //EF_DELAYED_VOLUME,  	// // // Delayed channel volume
	  $0, //EF_FDS_MOD_DEPTH,   	// FDS modulation depth
	  $0, //EF_FDS_MOD_SPEED_HI,	// FDS modulation speed hi
	  $0, //EF_FDS_MOD_SPEED_LO,	// FDS modulation speed lo
	  $0, //EF_DPCM_PITCH,      	// DPCM Pitch
	  $0, //EF_SUNSOFT_ENV_TYPE,	// Sunsoft envelope type
	  $0, //EF_SUNSOFT_ENV_HI,  	// Sunsoft envelope high
	  $0, //EF_SUNSOFT_ENV_LO,  	// Sunsoft envelope low
	  $0, //EF_SUNSOFT_NOISE,   	// // // 050B Sunsoft noise period
	  $0, //EF_VRC7_PORT,       	// // // 050B VRC7 custom patch port
	  $0, //EF_VRC7_WRITE,      	// // // 050B VRC7 custom patch write
	  $E, //EF_NOTE_RELEASE,    	// // // Delayed release
	  $0, //EF_GROOVE,          	// // // Groove
	  $0, //EF_TRANSPOSE,       	// // // Delayed transpose
	  $0, //EF_N163_WAVE_BUFFER,	// // // N163 wave buffer
	  $0, //EF_FDS_VOLUME,      	// // // FDS volume envelope
	  $0, //EF_FDS_MOD_BIAS,    	// // // FDS auto-FM bias
	  $0, //EF_PHASE_RESET,  // Reset waveform phase without retriggering note (VRC6-only so far)
	  $0, //EF_HARMONIC,  // Multiply the note pitch by an integer
    $0 //EF_COUNT
  );

type
  // PatternEditorTypes.h
  {$PACKENUM 4} // https://www.freepascal.org/docs-html/ref/refsu4.html
  TFTMColumn = (
	  COLUMN_NOTE = 0,
	  COLUMN_INSTRUMENT = 1,
	  COLUMN_VOLUME = 2,
	  COLUMN_EFF1 = 3,
	  COLUMN_EFF2 = 4,
	  COLUMN_EFF3 = 5,
	  COLUMN_EFF4 = 6
  );
  {$PACKENUM 1}

  // PatternEditorTypes.h
  TFTMClipInfo = packed record
    Channels: Integer;
    Rows: Integer;
    StartColumn: TFTMColumn;
    EndColumn: TFTMColumn;
    OleInfo: record
      ChanOffset: Integer;
      RowOffset: Integer;
    end;
  end;

  // PatternNote.h
  TFTMChanNote = packed record
    Note: Byte;
    Octave: Byte;
    Vol: Byte;
    Instrument: Byte;
    EffNumber: array[0..3] of Byte;
    EffParam: array[0..3] of Byte;
  end;

function GetFamitrackerPastedCells: TSelection;
  function FTMNoteToCell(Note: TFTMChanNote): TCell;
  begin
    case Note.Note of
      0, 13, 14, 15: Result.Note := NO_NOTE;
      else Result.Note := (C_3 + (Note.Note-1)) + (12*(Note.Octave - 1));
    end;

    if Note.Instrument = 64 then
      Result.Instrument := 0
    else
      Result.Instrument := Note.Instrument;

    if Note.Note in [13, 14, 15] then begin
      Result.EffectCode := $E;
      Result.EffectParams.Value := $00;
    end else begin
      Result.EffectCode := FTM_EFFECTS[Note.EffNumber[0]];
      Result.EffectParams.Value := Note.EffParam[0];
    end;

    if  (Result.EffectCode = 0)
    and (Result.EffectParams.Value = 0)
    and InRange(Note.Vol, 0, $F) then begin
      Result.EffectCode := $C;
      Result.EffectParams.Param2 := Note.Vol;
    end;
  end;
  function FTMColumnToPart(Col: TFTMColumn): TCellPart;
  begin
    case Col of
      COLUMN_NOTE: Exit(cpNote);
      COLUMN_INSTRUMENT: Exit(cpInstrument);
      COLUMN_VOLUME: Exit(cpVolume);
      COLUMN_EFF1..COLUMN_EFF4: Exit(cpEffectParams);
    end;
  end;
var
  ClipInfo: TFTMClipInfo;
  FTMNotes: packed array of TFTMChanNote;
  S: TMemoryStream;
  CH, I: Integer;
begin
  S := TMemoryStream.Create;
  try
    Clipboard.GetFormat(Clipboard.FindFormatID('FamiTracker Pattern'), S);
    S.Seek(0, soBeginning);

    S.Read(ClipInfo, SizeOf(ClipInfo));
    SetLength(FTMNotes, ClipInfo.Channels * ClipInfo.Rows);
    S.Read(FTMNotes[0], SizeOf(TFTMChanNote) * ClipInfo.Channels * ClipInfo.Rows);

    SetLength(Result, ClipInfo.Rows);

    for I := 0 to ClipInfo.Rows-1 do begin
      SetLength(Result[I], ClipInfo.Channels);
      for CH := 0 to ClipInfo.Channels-1 do begin
        Result[I, CH].Cell := FTMNoteToCell(FTMNotes[(CH * ClipInfo.Rows) + I]);
        Result[I, CH].Parts := [cpNote, cpInstrument, cpVolume, cpEffectCode, cpEffectParams];
      end;
    end;

    for I := 0 to ClipInfo.Rows-1 do begin
      Result[I, 0].Parts *= [FTMColumnToPart(ClipInfo.StartColumn)..cpEffectParams];
      Result[I, ClipInfo.Channels-1].Parts *= [cpNote..FTMColumnToPart(ClipInfo.EndColumn)];
    end;
  finally
    S.Free;
  end;
end;

function ParseCell(Cell: String): TSelectedCell;
  function StrToInt_(S: String; out I: Integer; Hex: Boolean = False): Boolean;
  begin
    if Trim(S) = '' then Exit(False);
    if Hex then S := 'x'+S;
    if not TryStrToInt(S, I) then
      I := 0;
    Result := True;
  end;
var
  Note, Instr, EffectCode, EffectParam: String;
  Temp: Integer;
begin
  Note := Cell.Substring(0, 3);
  Instr := Cell.Substring(3, 2);
  EffectCode := Cell.Substring(8, 1);
  EffectParam := Cell.Substring(9, 2);

  Result.Parts := [cpNote, cpInstrument, cpEffectCode, cpEffectParams];

  if Trim(Note) = '' then Exclude(Result.Parts, cpNote)
  else if not NoteToCodeMap.TryGetData(Note, Result.Cell.Note) then
    Result.Cell.Note := NO_NOTE;

  if not StrToInt_(Instr, Result.Cell.Instrument) then
    Exclude(Result.Parts, cpInstrument);
  if not StrToInt_(EffectCode, Result.Cell.EffectCode, True) then
    Exclude(Result.Parts, cpEffectCode);
  if StrToInt_(EffectParam, Temp, True) then
    Result.Cell.EffectParams.Value := Temp
  else
    Exclude(Result.Parts, cpEffectParams);
end;

function GetPastedCells: TSelection;
var
  SL: TStringList;
  StringCells: TStringArray;
  Row: String;
  I, J: Integer;
begin
  if Clipboard.HasFormatName('FamiTracker Pattern') then
    Exit(GetFamitrackerPastedCells);

  SL := TStringList.Create;

  try
    try
      SL.Text := Clipboard.AsText;

      // Delete lines until we reach the note data
      while not SL.Strings[0].StartsWith('|') do
        SL.Delete(0);
      SetLength(Result, SL.Count);

      I := 0;
      for Row in SL do begin
        StringCells := Row.Split('|');

        SetLength(Result[I], Length(StringCells)-1);
        for J := 0 to High(StringCells)-1 do
          Result[I, J] := ParseCell(StringCells[J+1]);
        Inc(I);
      end;
    except
      on E: Exception do
        raise EClipboardFormatException.Create('Clipboard contained invalid data!');
    end;
  finally
    SL.Free;
  end;
end;

function SerializeCell(Cell: TSelectedCell): String;
begin
  Result := '|';

  if cpNote in Cell.Parts then
    Result += NoteCodeToString(Cell.Cell.Note)
  else
    Result += '   ';

  if cpInstrument in Cell.Parts then
    Result += FormatFloat('00', Cell.Cell.Instrument)
  else
    Result += '  ';

  Result += '...'; // volume

  if (cpEffectCode in Cell.Parts) or (cpEffectParams in Cell.Parts) then
    Result += EffectCodeToStr(Cell.Cell.EffectCode, Cell.Cell.EffectParams)
  else
    Result += '   ';
end;

procedure CopyCells(Selection: TSelection);
var
  C, R: Integer;
  S: String;
  SL: TStringList;
begin
  SL := TStringList.Create;
  SL.Add('The hUGETracker paste format is compatible with...');
  SL.Add('ModPlug Tracker  XM');
  try
    for R := 0 to High(Selection) do begin
      S := '';
      for C := 0 to High(Selection[R]) do
        S += SerializeCell(Selection[R, C]);
      SL.Add(S);
    end;

    Clipboard.AsText := SL.Text;
  finally
    SL.Free;
  end;
end;

end.

