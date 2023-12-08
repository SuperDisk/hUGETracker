unit about_hugetracker;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfrmAboutHugetracker }

  TfrmAboutHugetracker = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    VersionLabel: TLabel;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TfrmAboutHugetracker.FormCreate(Sender: TObject);
var
  GitVersion: String;
begin
  GitVersion := {$I %VERSION_STRING%};
  if GitVersion = '' then
    VersionLabel.Caption := 'Unknown Version'
  else
    VersionLabel.Caption := 'Version '+GitVersion;
end;

procedure TfrmAboutHugetracker.FormDeactivate(Sender: TObject);
begin
  Close;
end;

end.

