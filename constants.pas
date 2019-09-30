unit Constants;

{$mode objfpc}

interface

uses
  fgl;

type
  TNoteMap = specialize TFPGMap<Integer, String>;
  TKeybindings = specialize TFPGmap<Word, Integer>;

const
  // Sound controller registers
  NR10 = $FF10;
  NR11 = $FF11;
  NR12 = $FF12;
  NR13 = $FF13;
  NR14 = $FF14;
  NR21 = $FF16;
  NR22 = $FF17;
  NR23 = $FF18;
  NR24 = $FF19;
  NR30 = $FF1A;
  NR31 = $FF1B;
  NR32 = $FF1C;
  NR33 = $FF1D;
  NR34 = $FF1E;
  NR41 = $FF20;
  NR42 = $FF21;
  NR43 = $FF22;
  NR44 = $FF23;

  // Note constants. These will probably go in a dictionary instead later
  C_3  =  44;
  CS3  =  156;
  D_3  =  262;
  DS3  =  363;
  E_3  =  457;
  F_3  =  547;
  FS3  =  631;
  G_3  =  710;
  GS3  =  786;
  A_3  =  854;
  AS3  =  923;
  B_3  =  986;
  C_4  =  1046;
  CS4  =  1102;
  D_4  =  1155;
  DS4  =  1205;
  E_4  =  1253;
  F_4  =  1297;
  FS4  =  1339;
  G_4  =  1379;
  GS4  =  1417;
  A_4  =  1452;
  AS4  =  1486;
  B_4  =  1517;
  C_5  =  1546;
  CS5  =  1575;
  D_5  =  1602;
  DS5  =  1627;
  E_5  =  1650;
  F_5  =  1673;
  FS5  =  1694;
  G_5  =  1714;
  GS5  =  1732;
  A_5  =  1750;
  AS5  =  1767;
  B_5  =  1783;
  C_6  =  1798;
  CS6  =  1812;
  D_6  =  1825;
  DS6  =  1837;
  E_6  =  1849;
  F_6  =  1860;
  FS6  =  1871;
  G_6  =  1881;
  GS6  =  1890;
  A_6  =  1899;
  AS6  =  1907;
  B_6  =  1915;
  C_7  =  1923;
  CS7  =  1930;
  D_7  =  1936;
  DS7  =  1943;
  E_7  =  1949;
  F_7  =  1954;
  FS7  =  1959;
  G_7  =  1964;
  GS7  =  1969;
  A_7  =  1974;
  AS7  =  1978;
  B_7  =  1982;
  C_8  =  1985;
  CS8  =  1988;
  D_8  =  1992;
  DS8  =  1995;
  E_8  =  1998;
  F_8  =  2001;
  FS8  =  2004;
  G_8  =  2006;
  GS8  =  2009;
  A_8  =  2011;
  AS8  =  2013;
  B_8  =  2015;

var
  NoteMap: TNoteMap;
  Keybindings: TKeybindings;

implementation

uses SysUtils;

begin
  NoteMap := TNoteMap.Create;
  Keybindings := TKeybindings.Create;

  Keybindings.Add(Ord('Q'), C_4);
  Keybindings.Add(Ord('W'), CS4);
  Keybindings.Add(Ord('E'), D_4);
  Keybindings.Add(Ord('R'), DS4);
  Keybindings.Add(Ord('T'), E_4);
  Keybindings.Add(Ord('Y'), F_4);
  Keybindings.Add(Ord('U'), FS4);
  Keybindings.Add(Ord('I'), G_4);
  Keybindings.Add(Ord('O'), GS4);
  Keybindings.Add(Ord('P'), A_4);
  Keybindings.Add(Ord('['), AS4);
  Keybindings.Add(Ord(']'), B_4);
  Keybindings.Add(Ord('\'), B_5);
  Keybindings.Add(Ord('A'), C_5);
  Keybindings.Add(Ord('S'), CS5);
  Keybindings.Add(Ord('D'), D_5);
  Keybindings.Add(Ord('F'), DS5);
  Keybindings.Add(Ord('G'), E_5);
  Keybindings.Add(Ord('H'), F_5);
  Keybindings.Add(Ord('J'), FS5);
  Keybindings.Add(Ord('K'), G_5);
  Keybindings.Add(Ord('L'), GS5);
  Keybindings.Add(Ord(';'), A_5);
  Keybindings.Add(Ord(''''), AS5);
  Keybindings.Add(Ord('Z'), C_6);
  Keybindings.Add(Ord('X'), CS6);
  Keybindings.Add(Ord('C'), D_6);
  Keybindings.Add(Ord('V'), DS6);
  Keybindings.Add(Ord('B'), E_6);
  Keybindings.Add(Ord('N'), F_6);
  Keybindings.Add(Ord('M'), FS6);
  Keybindings.Add(Ord(','), G_6);
  Keybindings.Add(Ord('.'), GS6);
  Keybindings.Add(Ord('/'), A_6);

  NoteMap.Add(C_4, 'C_4');
  NoteMap.Add(CS4, 'C#4');
  NoteMap.Add(D_4, 'D_4');
  NoteMap.Add(DS4, 'D#4');
  NoteMap.Add(E_4, 'E_4');
  NoteMap.Add(F_4, 'F_4');
  NoteMap.Add(FS4, 'F#4');
  NoteMap.Add(G_4, 'G_4');
  NoteMap.Add(GS4, 'G#4');
  NoteMap.Add(A_4, 'A_4');
  NoteMap.Add(AS4, 'A#4');
  NoteMap.Add(B_4, 'B_4');
  NoteMap.Add(B_5, 'B_5');
  NoteMap.Add(C_5, 'C_5');
  NoteMap.Add(CS5, 'C#5');
  NoteMap.Add(D_5, 'D_5');
  NoteMap.Add(DS5, 'D#5');
  NoteMap.Add(E_5, 'E_5');
  NoteMap.Add(F_5, 'F_5');
  NoteMap.Add(FS5, 'F#5');
  NoteMap.Add(G_5, 'G_5');
  NoteMap.Add(GS5, 'G#5');
  NoteMap.Add(A_5, 'A_5');
  NoteMap.Add(AS5, 'A#5');
  NoteMap.Add(C_6, 'C_6');
  NoteMap.Add(CS6, 'C#6');
  NoteMap.Add(D_6, 'D_6');
  NoteMap.Add(DS6, 'D#6');
  NoteMap.Add(E_6, 'E_6');
  NoteMap.Add(F_6, 'F_6');
  NoteMap.Add(FS6, 'F#6');
  NoteMap.Add(G_6, 'G_6');
  NoteMap.Add(GS6, 'G#6');
  NoteMap.Add(A_6, 'A_6');
end.

