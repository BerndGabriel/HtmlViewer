{Version 9.45}

{$I htmlcons.inc}  

unit HTMLCompEdit;

interface

uses
  SysUtils, Windows, Messages, Classes, Controls, StdCtrls,
  Dialogs, ExtCtrls,
  {$ifdef Delphi6_Plus}
  designintf, DesignEditors;        
  {$else}
  dsgnintf;
  {$endif}


type
  THTMLEditor = class(TComponentEditor)
    function GetVerbCount: Integer; Override;
    function GetVerb(index: Integer): String; Override;
    procedure ExecuteVerb(index: Integer); Override;
    end;

  TFMVEditor = class(TComponentEditor)
    function GetVerbCount: Integer; Override;
    function GetVerb(index: Integer): String; Override;
    procedure ExecuteVerb(index: Integer); Override;
    end;

  TFMBEditor = class(TComponentEditor)
    function GetVerbCount: Integer; Override;
    function GetVerb(index: Integer): String; Override;
    procedure ExecuteVerb(index: Integer); Override;
    end;

procedure Register;

implementation

uses
  htmlview, htmlun2, framview, frambrwz;

function THTMLEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

function THTMLEditor.GetVerb(index: Integer): string;
begin
  Result := 'About..';
end;

procedure THTMLEditor.ExecuteVerb(index:integer);
begin
  MessageDlg('ThtmlViewer'+#13#13+
             'Version     : '+VersionNo+#13#13+
             'Copyright  : 1995-2008 by L. David Baldwin'+#13#13
             ,mtInformation,[mbOk],0)
end;

{----------------TFMVEditor.GetVerbCount:}
function TFMVEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

function TFMVEditor.GetVerb(index: Integer): string;
begin
  Result := 'About..';
end;

procedure TFMVEditor.ExecuteVerb(index:integer);
begin
  MessageDlg('TFrameViewer'+#13#13+
             'Version     : '+VersionNo+#13#13+
             'Copyright  : 1995-2008 by L. David Baldwin'+#13#13
             ,mtInformation,[mbOk],0)
end;

{----------------TFMBEditor.GetVerbCount:}
function TFMBEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

function TFMBEditor.GetVerb(index: Integer): string;
begin
  Result := 'About..';
end;

procedure TFMBEditor.ExecuteVerb(index:integer);
begin
  MessageDlg('TFrameBrowser'+#13#13+
             'Version     : '+VersionNo+#13#13+
             'Copyright  : 1995-2008 by L. David Baldwin'+#13#13
             ,mtInformation,[mbOk],0)
end;

procedure Register;
begin
RegisterComponentEditor(THTMLViewer, THTMLEditor);
RegisterComponentEditor(TFrameViewer, TFMVEditor);
RegisterComponentEditor(TFrameBrowser, TFMBEditor);
end;

end.
