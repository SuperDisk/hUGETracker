unit about_hugetracker;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfrmAboutHugetracker }

  TfrmAboutHugetracker = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private

  public

  end;

var
  frmAboutHugetracker: TfrmAboutHugetracker;

implementation

{$R *.lfm}

{ TfrmAboutHugetracker }

procedure TfrmAboutHugetracker.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmAboutHugetracker.FormDeactivate(Sender: TObject);
begin
  Close;
end;

end.

