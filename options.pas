unit options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    Button1: TButton;
    ScopesCheck: TCheckBox;
    Label1: TLabel;
    FontSizeSpinner: TSpinEdit;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  frmOptions: TfrmOptions;

implementation

{$R *.lfm}

{ TfrmOptions }

procedure TfrmOptions.Button1Click(Sender: TObject);
begin
  Close;
end;

end.

