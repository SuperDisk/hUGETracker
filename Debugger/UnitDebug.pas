{+-----------------------------------------------------------------------------+
 | Author:      Christian Hackbart
 | Description: Debugger functions 
 | Copyright (c) 2000 Christian Hackbart
 | last Changes: 31.10.2000
 |
 | http://www.tu-ilmenau.de/~hackbart
 +----------------------------------------------------------------------------+
 | nothing done yet :(
 | - should be a dockable window
 +----------------------------------------------------------------------------+}
unit UnitDebug;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  debugger, StdCtrls, ComCtrls, Menus;

type
  TfrmDebug = class(TForm)
  private
    { Private-Deklarationen}
  public
    { Public-Deklarationen}
  end;

var
  frmDebug: TfrmDebug;

implementation

{$R *.lfm}

end.
