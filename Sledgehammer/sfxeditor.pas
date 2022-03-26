unit SFXEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  ComCtrls, Menus, Spin, SFXGrid, hUGESettings, options,
  HugeDatatypes, Keymap, About, Constants, Math;

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
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
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
    SpinEdit1: TSpinEdit;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure HeaderControl1Resize(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);

    procedure RecreateRowNumbers;
    procedure RecreateSFXGrid;
    procedure CreateKeymap;

    procedure BlankSFXPattern(P: PPattern);

    procedure LoadSFXBank(Filename: String);
    procedure LoadSFX(S: TStream; Num: Integer);
    procedure DisplaySFX(Num: Integer);
    procedure ExportSFXToC(FX: THammerFX; Filename: String);
  private
    SFXGrid: TSFXGrid;
    SFXPatterns: TPatternMap;
    FX: array[0..$3B] of THammerFX;
    CurrentFX: Integer;
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

procedure TSFXEditor1.HeaderControl1Resize(Sender: TObject);
var
  I: Integer;
begin
  // Fix the size of the channel headers
  HeaderControl1.Sections.Items[1].Width := SFXGrid.LenColumnWidth;
  for I := 2 to HeaderControl1.Sections.Count-1 do
    HeaderControl1.Sections.Items[I].Width := SFXGrid.ColumnWidth;
end;

procedure TSFXEditor1.MenuItem10Click(Sender: TObject);
begin
end;

procedure TSFXEditor1.MenuItem12Click(Sender: TObject);
begin

end;

procedure TSFXEditor1.MenuItem5Click(Sender: TObject);
begin

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

procedure TSFXEditor1.LoadSFXBank(Filename: String);
var
  S: TFileStream;
  I: Integer;
begin
  S := TFileStream.Create(Filename, fmOpenRead);

  try
    for I := Low(FX) to High(FX) do
      LoadSFX(S, I);
  finally
    S.Free;
  end;
end;

procedure TSFXEditor1.LoadSFX(S: TStream; Num: Integer);
var
  Frame: TFXFrame;
begin
  // https://github.com/datguywitha3ds/CBT-FX/blob/main/hammer2cbt.py

  with FX[Num] do begin
    S.Seek($200 + Num, soBeginning);
    Priority := S.ReadByte;

    S.Seek($300 + Num, soBeginning);
    CHUsed := S.ReadByte;

    S.Seek($400 + (Num * 256), soBeginning);

    Frames := nil;
    repeat
      S.ReadBuffer(Frame, SizeOf(TFXFrame));
      SetLength(Frames, Length(Frames)+1);
      Frames[High(Frames)] := Frame;
    until Frame.Duration = 0;
    SetLength(Frames, Length(Frames)-1);
  end;
end;

procedure TSFXEditor1.DisplaySFX(Num: Integer);
var
  I: Integer;
  Pat: PPattern;
  PolyCounter: TPolynomialCounterRegister;
  CH4Freq: Integer;
  RealR: Double;
begin
  with FX[Num] do
    for I := Low(Frames) to High(Frames) do begin
      Pat := SFXPatterns.GetOrCreateNew(0);
      Pat^[I].EffectParams.Value := Frames[I].Duration;

      Pat := SFXPatterns.GetOrCreateNew(1);

      Pat^[I].Note := (Frames[I].CH2Note - $40) div 2;
      Pat^[I].Volume := (Frames[I].CH2Vol and $F0) shr 4;
      Pat^[I].Instrument := (Frames[I].CH2Duty and %11000000) shr 6;

      Pat^[I].EffectCode := $00;
      if (Frames[I].CH2Pan and $F0) <> 0 then
        Pat^[I].EffectCode := (Pat^[I].EffectCode or $F0);
      if (Frames[I].CH2Pan and $0F) <> 0 then
        Pat^[I].EffectCode := (Pat^[I].EffectCode or $0F);

      Pat := SFXPatterns.GetOrCreateNew(2);

      if Frames[I].CH4Freq <> 0 then begin
        PolyCounter.ByteValue := Frames[I].CH4Freq;

        if PolyCounter.DividingRatio = 0 then
          RealR := 0.5
        else
          RealR := PolyCounter.DividingRatio;

        Ch4Freq := Trunc((524288 / RealR) / 2**(PolyCounter.ShiftClockFrequency+1));
        Ch4FreqToNoteCodeMap.TryGetData(Ch4Freq, Pat^[I].Note);
      end;

      Pat^[I].Volume := (Frames[I].CH4Vol and $F0) shr 4;

      Pat^[I].EffectCode := $00;
      if (Frames[I].CH4Pan and $F0) <> 0 then
        Pat^[I].EffectCode := (Pat^[I].EffectCode or $F0);
      if (Frames[I].CH4Pan and $0F) <> 0 then
        Pat^[I].EffectCode := (Pat^[I].EffectCode or $0F);
    end;

  SFXGrid.Invalidate;
end;

procedure TSFXEditor1.ExportSFXToC(FX: THammerFX; Filename: String);
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
  // https://github.com/datguywitha3ds/CBT-FX/blob/main/hammer2cbt.py

  SL := TStringList.Create;
  SL.Delimiter:=',';

  AssignFile(F, Filename);
  Rewrite(F);

  Chu := 0;
  if (FX.CHUsed and $30) <> 0 then
    Chu := Chu or 12;
  if (FX.CHUsed and $03) <> 0 then
    Chu := Chu or 3;
  Chu := Chu shl 4;

  Writeln(F, 'const unsigned char', '[] = {');
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

procedure TSFXEditor1.MenuItem7Click(Sender: TObject);
begin
  frmOptions.ShowModal;
end;

procedure TSFXEditor1.MenuItem8Click(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TSFXEditor1.MenuItem9Click(Sender: TObject);
begin
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

