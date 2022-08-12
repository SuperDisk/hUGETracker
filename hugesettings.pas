unit hUGESettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, INIFiles;

type

  { TTrackerSettings }

  TTrackerSettings = class
  private
    FDisplayRowNumbersAsHex: Boolean;
    FDisplayOrderRowNumbersAsHex: Boolean;
    FPreviewWhenBumping: Boolean;
    FPreviewWhenPlacing: Boolean;
    SettingsFile: TINIFile;

    FPatternEditorFontSize: Integer;
    FUseScopes, FUseCustomKeymap: Boolean;
    FAlwaysCenterActiveRow: Boolean;

    procedure SetAlwaysCenterActiveRow(AValue: Boolean);
    procedure SetDisplayOrderRowNumbersAsHex(AValue: Boolean);
    procedure SetDisplayRowNumbersAsHex(AValue: Boolean);
    procedure SetPatternEditorFontSize(AValue: Integer);
    procedure SetPreviewWhenBumping(AValue: Boolean);
    procedure SetPreviewWhenPlacing(AValue: Boolean);
    procedure SetUseCustomKeymap(AValue: Boolean);
    procedure SetUseScopes(AValue: Boolean);
  public
    property PatternEditorFontSize: Integer read FPatternEditorFontSize write SetPatternEditorFontSize;
    property UseScopes: Boolean read FUseScopes write SetUseScopes;
    property UseCustomKeymap: Boolean read FUseCustomKeymap write SetUseCustomKeymap;
    property PreviewWhenPlacing: Boolean read FPreviewWhenPlacing write SetPreviewWhenPlacing;
    property PreviewWhenBumping: Boolean read FPreviewWhenBumping write SetPreviewWhenBumping;
    property DisplayRowNumbersAsHex: Boolean read FDisplayRowNumbersAsHex write SetDisplayRowNumbersAsHex;
    property DisplayOrderRowNumbersAsHex: Boolean read FDisplayOrderRowNumbersAsHex write SetDisplayOrderRowNumbersAsHex;
    property AlwaysCenterActiveRow: Boolean read FAlwaysCenterActiveRow write SetAlwaysCenterActiveRow;


    constructor Create;
  end;

procedure InitializeTrackerSettings;

var
  TrackerSettings: TTrackerSettings = nil;

implementation

procedure InitializeTrackerSettings;
begin
  TrackerSettings := TTrackerSettings.Create;
end;

{ TTrackerSettings }

procedure TTrackerSettings.SetPatternEditorFontSize(AValue: Integer);
begin
  FPatternEditorFontSize := AValue;
  SettingsFile.WriteInteger('hUGETracker', 'fontsize', AValue);
end;

procedure TTrackerSettings.SetDisplayRowNumbersAsHex(AValue: Boolean);
begin
  FDisplayRowNumbersAsHex:=AValue;
  SettingsFile.WriteBool('hUGETracker', 'DisplayRowNumbersAsHex', AValue);
end;

procedure TTrackerSettings.SetDisplayOrderRowNumbersAsHex(AValue: Boolean);
begin
  FDisplayOrderRowNumbersAsHex:=AValue;
  SettingsFile.WriteBool('hUGETracker', 'DisplayOrderRowNumbersAsHex', AValue);
end;

procedure TTrackerSettings.SetAlwaysCenterActiveRow(AValue: Boolean);
begin
  FAlwaysCenterActiveRow:=AValue;
  SettingsFile.WriteBool('hUGETracker', 'AlwaysCenterActiveRow', AValue);
end;

procedure TTrackerSettings.SetPreviewWhenBumping(AValue: Boolean);
begin
  FPreviewWhenBumping:=AValue;
  SettingsFile.WriteBool('hUGETracker', 'PreviewWhenBumping', AValue);
end;

procedure TTrackerSettings.SetPreviewWhenPlacing(AValue: Boolean);
begin
  FPreviewWhenPlacing:=AValue;
  SettingsFile.WriteBool('hUGETracker', 'PreviewWhenPlacing', AValue);
end;

procedure TTrackerSettings.SetUseCustomKeymap(AValue: Boolean);
begin
  FUseCustomKeymap := AValue;
  SettingsFile.WriteBool('hUGETracker', 'CustomKeymap', AValue);
end;

procedure TTrackerSettings.SetUseScopes(AValue: Boolean);
begin
  FUseScopes := AValue;
  SettingsFile.WriteBool('hUGETracker', 'ScopesOn', AValue);
end;

constructor TTrackerSettings.Create;
begin
  SettingsFile := TINIFile.Create('options.ini');

  FPatternEditorFontSize := SettingsFile.ReadInteger('hUGETracker', 'fontsize', 12);
  FUseScopes := SettingsFile.ReadBool('hUGETracker', 'ScopesOn', True);
  FUseCustomKeymap := SettingsFile.ReadBool('hUGETracker', 'CustomKeymap', False);
  FPreviewWhenPlacing := SettingsFile.ReadBool('hUGETracker', 'PreviewWhenPlacing', True);
  FPreviewWhenBumping := SettingsFile.ReadBool('hUGETracker', 'PreviewWhenBumping', False);
  FDisplayRowNumbersAsHex := SettingsFile.ReadBool('hUGETracker', 'DisplayRowNumbersAsHex', False);
  FDisplayOrderRowNumbersAsHex := SettingsFile.ReadBool('hUGETracker', 'DisplayOrderRowNumbersAsHex', False);
  FAlwaysCenterActiveRow := SettingsFile.ReadBool('hUGETracker', 'AlwaysCenterActiveRow', False);
end;

finalization
  if Assigned(TrackerSettings) then TrackerSettings.Free;

end.

