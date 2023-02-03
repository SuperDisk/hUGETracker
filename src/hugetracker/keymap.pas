unit Keymap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Constants, LCLType, LCLProc, Grids;

type
  TKeybindings = specialize TFPGmap<Word, Integer>;

procedure LoadDefaultKeybindings;
procedure LoadCustomKeybindings(Grid: TStringGrid);

var
  Keybindings: TKeybindings;

implementation

procedure LoadDefaultKeybindings;
begin
  Keybindings.Clear;

  Keybindings.Add(VK_Q, C_3);
  Keybindings.Add(VK_W, CS3);
  Keybindings.Add(VK_E, D_3);
  Keybindings.Add(VK_R, DS3);
  Keybindings.Add(VK_T, E_3);
  Keybindings.Add(VK_Y, F_3);
  Keybindings.Add(VK_U, FS3);
  Keybindings.Add(VK_I, G_3);
  Keybindings.Add(VK_O, GS3);
  Keybindings.Add(VK_P, A_3);
  Keybindings.Add(VK_OEM_4, AS3);
  Keybindings.Add(VK_OEM_6, B_3);
  Keybindings.Add(VK_OEM_5, B_4);
  Keybindings.Add(VK_A, C_4);
  Keybindings.Add(VK_S, CS4);
  Keybindings.Add(VK_D, D_4);
  Keybindings.Add(VK_F, DS4);
  Keybindings.Add(VK_G, E_4);
  Keybindings.Add(VK_H, F_4);
  Keybindings.Add(VK_J, FS4);
  Keybindings.Add(VK_K, G_4);
  Keybindings.Add(VK_L, GS4);
  Keybindings.Add(VK_OEM_1, A_4);
  Keybindings.Add(VK_OEM_7, AS4);
  Keybindings.Add(VK_Z, C_5);
  Keybindings.Add(VK_X, CS5);
  Keybindings.Add(VK_C, D_5);
  Keybindings.Add(VK_V, DS5);
  Keybindings.Add(VK_B, E_5);
  Keybindings.Add(VK_N, F_5);
  Keybindings.Add(VK_M, FS5);
  Keybindings.Add(VK_OEM_COMMA, G_5);
  Keybindings.Add(VK_OEM_PERIOD, GS5);
  Keybindings.Add(VK_OEM_2, A_5);
end;

procedure LoadCustomKeybindings(Grid: TStringGrid);
var
  I: Integer;
  SC: TShortCut;
begin
  Keybindings.Clear;

  for I := 1 to Grid.RowCount-1 do begin
    SC := TextToShortCut(Grid.Cells[0, I]);

    if SC = 0 then Continue;
    if NoteToCodeMap.IndexOf(Grid.Cells[1, I]) = -1 then Continue;

    Keybindings.Add(SC, NoteToCodeMap.KeyData[Grid.Cells[1, I]]);
  end;
end;

begin
  Keybindings := TKeybindings.Create;
end.

