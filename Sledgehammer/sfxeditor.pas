unit SFXEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  ComCtrls, Menus, SFXGrid, hUGESettings, options, HugeDatatypes, Keymap, About,
  Constants, Math;

type
  { TFXFrame }

  TFXFrame = packed record
    Duration: Byte;

    CH2Pan: Byte;
    CH2Vol: Byte;
    CH2Duty: Byte;
    CH2Note: Byte;

    CH4Pan: Byte;
    CH4Vol: Byte;
    CH4Freq: Byte;
  end;

  { THammerFX }

  THammerFX = packed record
    Priority: Byte;
    CHUsed: Byte;
    Frames: array of TFXFrame;
  end;


  { TSFXEditor1 }

  TSFXEditor1 = class(TForm)
    HeaderControl1: THeaderControl;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    RowNumberStringGrid: TStringGrid;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);

    procedure RecreateRowNumbers;
    procedure RecreateSFXGrid;
    procedure CreateKeymap;

    procedure BlankSFXPattern(P: PPattern);
  private
    SFXGrid: TSFXGrid;
    SFXPatterns: TPatternMap;
    FX: THammerFX;
  public

  end;

var
  SFXEditor1: TSFXEditor1;

implementation

{$R *.lfm}

{ TSFXEditor1 }

procedure TSFXEditor1.FormCreate(Sender: TObject);
begin
  SFXPatterns := TPatternMap.Create;

  RecreateSFXGrid;
  RecreateRowNumbers;
  CreateKeymap;

  BlankSFXPattern(SFXPatterns.GetOrCreateNew(0));
  BlankSFXPattern(SFXPatterns.GetOrCreateNew(1));
  BlankSFXPattern(SFXPatterns.GetOrCreateNew(2));

  SFXGrid.LoadPattern(0, 0);
  SFXGrid.LoadPattern(1, 1);
  SFXGrid.LoadPattern(2, 2);
end;

procedure TSFXEditor1.MenuItem10Click(Sender: TObject);
var
  F: Text;
  SL: TStringList;
  I: Integer;
  Chu: Integer;

  procedure Put(B: Byte);
  begin
    SL.Add(IntToStr(B));
  end;
begin
  SL := TStringList.Create;
  SL.Delimiter:=',';

  AssignFile(F, '/home/superdisk/Desktop/asdf.c');
  Rewrite(F);

  if (FX.CHUsed and $30) <> 0 then
    Chu := Chu or 12;
  if (FX.CHUsed and $03) <> 0 then
    Chu := Chu or 3;
  Chu := Chu shl 4;

  Writeln(F, 'const unsigned char asdf[] = {');
  Put(Chu or FX.Priority);
  Put(Length(FX.Frames));

  for I := Low(FX.Frames) to High(FX.Frames) do begin
    with FX.Frames[I] do begin
      // Duration & Pan
      Put(Duration - 1);
      Put(CH2Pan or CH4Pan);

      // CH2 Duty
      if (Chu and 128) <> 0 then begin
        Put(CH2Duty);
      end;

      // Volume
      Put(CH2Vol or (CH4Vol shr 4));

      // CH2 Frequency
      if (Chu and 128) <> 0 then begin
        Put(NotesToFreqs.KeyData[(CH2Note - $40) shr 1] and $FF);
        Put((NotesToFreqs.KeyData[(CH2Note - $40) shr 1] shr 8) or $80)
      end;

      // CH4 Frequency
      if (Chu and 32) <> 0 then
        Put(CH4Freq);
    end;
  end;

  Writeln(F, SL.DelimitedText);
  Writeln(F, '};');

  CloseFile(F);
  SL.Free;
end;

procedure TSFXEditor1.CreateKeymap;
var
  StringGrid: TStringGrid;
begin
  if not TrackerSettings.UseCustomKeymap then
    LoadDefaultKeybindings
  else begin
    StringGrid := TStringGrid.Create(nil); // UGH!
    try
      StringGrid.SaveOptions := soAll;
      StringGrid.LoadFromFile('custom_keymap.km');
      LoadCustomKeybindings(StringGrid);
    finally
      StringGrid.Free;
    end;
  end;
end;

procedure TSFXEditor1.BlankSFXPattern(P: PPattern);
var
  I: Integer;
begin
  for I := Low(P^) to High(P^) do begin
    P^[I] := Default(TCell);
    P^[I].EffectCode := $FF;
    P^[I].Note := NO_NOTE;
  end;
end;

procedure TSFXEditor1.MenuItem7Click(Sender: TObject);
begin
  frmOptions.ShowModal;
end;

procedure TSFXEditor1.MenuItem8Click(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TSFXEditor1.MenuItem9Click(Sender: TObject);
var
  S: TFileStream;
  Frame: TFXFrame;
  I: Integer;
  Pat: PPattern;
  PolyCounter: TPolynomialCounterRegister;
  CH4Freq: Integer;
  RealR: Double;
begin
  //ShowMessage(GetCurrentDir);
  S := TFileStream.Create('/home/superdisk/hUGETracker/lib/Development/x86_64-openbsd/fxbank.sav', fmOpenRead);

  S.Seek($200, soBeginning);
  FX.Priority := S.ReadByte;

  S.Seek($300, soBeginning);
  FX.CHUsed := S.ReadByte;

  S.Seek($400, soBeginning);

  FX.Frames := nil;
  repeat
    S.ReadBuffer(Frame, SizeOf(TFXFrame));
    SetLength(FX.Frames, Length(FX.Frames)+1);
    FX.Frames[High(FX.Frames)] := Frame;
  until Frame.Duration = 0;
  SetLength(FX.Frames, Length(FX.Frames)-1);

  for I := Low(FX.Frames) to High(FX.Frames) do begin
    Pat := SFXPatterns.GetOrCreateNew(0);
    Pat^[I].EffectParams.Value := FX.Frames[I].Duration;
    if FX.Frames[I].Duration = 0 then
      Continue;

    Pat := SFXPatterns.GetOrCreateNew(1);

    Pat^[I].Note := (FX.Frames[I].CH2Note - $40) div 2;
    Pat^[I].Volume := (FX.Frames[I].CH2Vol and $F0) shr 4;
    Pat^[I].Instrument := (FX.Frames[I].CH2Duty and %11000000) shr 6;

    Pat^[I].EffectCode := $00;
    if (FX.Frames[I].CH2Pan and $F0) <> 0 then
      Pat^[I].EffectCode := (Pat^[I].EffectCode or $F0);
    if (FX.Frames[I].CH2Pan and $0F) <> 0 then
      Pat^[I].EffectCode := (Pat^[I].EffectCode or $0F);

    Pat := SFXPatterns.GetOrCreateNew(2);

    if FX.Frames[I].CH4Freq <> 0 then begin
      PolyCounter.ByteValue := FX.Frames[I].CH4Freq;

      if PolyCounter.DividingRatio = 0 then
        RealR := 0.5
      else
        RealR := PolyCounter.DividingRatio;

      Ch4Freq := Trunc((524288 / RealR) / 2**(PolyCounter.ShiftClockFrequency+1));
      Ch4FreqToNoteCodeMap.TryGetData(Ch4Freq, Pat^[I].Note);
    end;

    Pat^[I].Volume := (FX.Frames[I].CH4Vol and $F0) shr 4;

    Pat^[I].EffectCode := $00;
    if (FX.Frames[I].CH4Pan and $F0) <> 0 then
      Pat^[I].EffectCode := (Pat^[I].EffectCode or $F0);
    if (FX.Frames[I].CH4Pan and $0F) <> 0 then
      Pat^[I].EffectCode := (Pat^[I].EffectCode or $0F);
  end;
  SFXGrid.Invalidate;

  S.Free;
end;

procedure TSFXEditor1.RecreateRowNumbers;
var
  I: Integer;
begin
  RowNumberStringGrid.Clean;
  // Add the row numbers to the string grid
  for I := 0 to RowNumberStringGrid.RowCount-1 do begin
    if TrackerSettings.DisplayRowNumbersAsHex then begin
      RowNumberStringGrid.Cells[0, I] := IntToHex(I, 2);
    end
    else begin
      RowNumberStringGrid.Cells[0, I] := IntToStr(I);
    end;
  end;
end;

procedure TSFXEditor1.RecreateSFXGrid;
var
  I: Integer;
begin
  if Assigned(SFXGrid) then SFXGrid.Free;

  SFXGrid := TSFXGrid.Create(Self, ScrollBox1, SFXPatterns, 3, 32);

  //TrackerGrid.OnResize:=@OnTrackerGridResize;
  //TrackerGrid.OnCursorOutOfBounds:=@OnTrackerGridCursorOutOfBounds;
  //TrackerGrid.FontSize := TrackerSettings.PatternEditorFontSize;
  SFXGrid.Left := RowNumberStringGrid.Left + RowNumberStringGrid.Width;
  //TrackerGrid.PopupMenu := TrackerGridPopup;
  RowNumberStringGrid.DefaultRowHeight := SFXGrid.RowHeight;

  // Fix the size of the channel headers
  HeaderControl1.Sections.Items[1].Width := SFXGrid.LenColumnWidth;
  for I := 2 to HeaderControl1.Sections.Count-1 do
    HeaderControl1.Sections.Items[I].Width := SFXGrid.ColumnWidth;
end;

end.

