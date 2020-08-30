unit hUGESettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, INIFiles;

type

  { TTrackerSettings }

  TTrackerSettings = class
  private
    SettingsFile: TINIFile;

    FPatternEditorFontSize: Integer;
    FUseScopes, FUseCustomKeymap: Boolean;

    procedure SetPatternEditorFontSize(AValue: Integer);
    procedure SetUseCustomKeymap(AValue: Boolean);
    procedure SetUseScopes(AValue: Boolean);
  public
    property PatternEditorFontSize: Integer read FPatternEditorFontSize write SetPatternEditorFontSize;
    property UseScopes: Boolean read FUseScopes write SetUseScopes;
    property UseCustomKeymap: Boolean read FUseCustomKeymap write SetUseCustomKeymap;

    constructor Create;
  end;

var
  TrackerSettings: TTrackerSettings = nil;

implementation

{ TTrackerSettings }

procedure TTrackerSettings.SetPatternEditorFontSize(AValue: Integer);
begin
  FPatternEditorFontSize := AValue;
  SettingsFile.WriteInteger('hUGETracker', 'fontsize', AValue);
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
end;

initialization
  TrackerSettings := TTrackerSettings.Create;
finalization
  if Assigned(TrackerSettings) then TrackerSettings.Free;

end.

