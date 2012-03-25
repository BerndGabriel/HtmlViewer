{
Version   11.2
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2011-2012 by Bernd Gabriel

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Note that the source modules HTMLGIF1.PAS and DITHERUNIT.PAS
are covered by separate copyright notices located in those modules.

ANGUS - fixed HotSpotClick not normalising URL before adding protocol for links

}

{$I htmlcons.inc}

unit FramBrwz;

interface

uses
{$ifdef LCL}
  LclIntf, LclType, LMessages, HtmlMisc,
{$else}
  Windows,
{$endif}
  SysUtils, Classes, Messages, Controls, ExtCtrls,
  HtmlGlobals, HtmlBuffer, HtmlSubs, HtmlView, Htmlun2, ReadHTML, UrlSubs, FramView;

type
  TGetPostRequestEvent = procedure(Sender: TObject; IsGet: boolean; const URL, Query: ThtString;
    Reload: boolean; var NewURL: ThtString; var DocType: ThtmlFileType; var Stream: TMemoryStream) of object;

  TGetPostRequestExEvent = procedure(Sender: TObject; IsGet: boolean; const URL, Query, EncType, Referer: ThtString;
    Reload: boolean; var NewURL: ThtString; var DocType: ThtmlFileType; var Stream: TMemoryStream) of object;
    
  TbrFormSubmitEvent = procedure(Sender: TObject; Viewer: ThtmlViewer;
    const Action, Target, EncType, Method: ThtString; Results: ThtStringList; var Handled: boolean) of object;

  TFrameBaseOpener = class(TFrameBase);

  TbrFrameSet = class;
  TbrSubFrameSet = class;

  TbrFrame = class(TViewerFrameBase) {TbrFrame holds a ThtmlViewer or TbrSubFrameSet}
  private
    URLBase: ThtString;
    TheStream: TMemoryStream;
    TheStreamType: ThtmlFileType;
  protected
    function ExpandSourceName(Base, Path: ThtString; S: ThtString): ThtString; override;
    function FrameSet: TbrSubFrameSet; {$ifdef UseInline} inline; {$endif}
    function GetSubFrameSetClass: TSubFrameSetClass; override;
    function MasterSet: TbrFrameSet; {$ifdef UseInline} inline; {$endif}
    procedure CreateViewer; override;
    procedure frLoadFromBrzFile(const URL, Dest, Query, EncType, Referer: ThtString; Bump, IsGet, Reload: boolean);
    procedure frLoadFromFile(const FName, Dest: ThtString; Bump, Reload: Boolean); override;
    procedure LoadFiles; override;
    procedure RefreshEvent(Sender: TObject; Delay: integer; const URL: ThtString); override;
    procedure RefreshTimerTimer(Sender: TObject); override;
    procedure ReLoadFiles(APosition: LongInt); override;
    procedure URLExpandName(Sender: TObject; const SRC: ThtString; var Rslt: ThtString);
  end;

  TbrSubFrameSet = class(TSubFrameSetBase) {can contain one or more TbrFrames and/or TSubFrameSets}
  private
    URLBase: ThtString;
  protected
    function GetFrameClass: TViewerFrameClass; override;
    procedure RefreshTimerTimer(Sender: Tobject); override;
  public
    constructor CreateIt(AOwner: TComponent; Master: TFrameSetBase); override;
  end;

  TFrameBrowser = class;

  TbrFrameSet = class(TFrameSetBase) {only one of these showing, others may be held as History}
  private
    URLBase: ThtString;
  protected
    function FrameViewer: TFrameBrowser; {$ifdef UseInline} inline; {$endif}
    function GetFrameClass: TViewerFrameClass; override;
    function MasterSet: TbrFrameSet; {$ifdef UseInline} inline; {$endif}
    function RequestEvent: Boolean; override;
    procedure RefreshTimerTimer(Sender: TObject); override;
    procedure LoadFromBrzFile(Stream: TMemoryStream; StreamType: ThtmlFileType; const URL, Dest: ThtString);
  end;

  TFrameBrowser = class(TFVBase)
  private
    FOnFormSubmit: TbrFormSubmitEvent;
    FOnGetPostRequest: TGetPostRequestEvent;
    FOnGetPostRequestEx: TGetPostRequestExEvent;
    FEncodePostArgs: boolean;
    InFormSubmit: boolean;
    function CurbrFrameSet: TbrFrameSet; {$ifdef UseInline} inline; {$endif} {the TbrFrameSet being displayed}
    procedure LoadURLInternal(const URL, Dest, Query, EncType, Referer: ThtString; IsGet, Reload: boolean);
  protected
    function GetFrameSetClass: TFrameSetClass; override;
    function GetSubFrameSetClass: TSubFrameSetClass; override;
    procedure AssertCanPostRequest(const URL: ThtString); virtual;
    procedure CheckVisitedLinks; override;
    procedure DoFormSubmitEvent(Sender: TObject; const Action, Target, EncType, Method: ThtString; Results: ThtStringList); override;
    procedure DoURLRequest(Sender: TObject; const SRC: ThtString; var Stream: TMemoryStream); override;
    procedure HotSpotCovered(Sender: TObject; const SRC: ThtString); override;
    procedure PostRequest(
      Sender: TObject;
      IsGet: boolean;
      const Source, Query, EncType, Referer: ThtString;
      Reload: boolean;
      out NewURL: ThtString;
      out DocType: ThtmlFileType;
      out Stream: TMemoryStream); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function GetViewerUrlBase(Viewer: ThtmlViewer): ThtString;
    procedure GetPostQuery(const URL, Query, EncType: ThtString; IsGet: boolean);
    procedure HotSpotClick(Sender: TObject; const AnURL: ThtString; var Handled: boolean); override;
    procedure LoadFromFile(const Name: ThtString); override;
    procedure LoadURL(const URL: ThtString);
    property EncodePostArgs: boolean read FEncodePostArgs write FEncodePostArgs;
  published
    property OnGetPostRequest: TGetPostRequestEvent read FOnGetPostRequest write FOnGetPostRequest;
    property OnGetPostRequestEx: TGetPostRequestExEvent read FOnGetPostRequestEx write FOnGetPostRequestEx;
    property OnFormSubmit: TbrFormSubmitEvent read FOnFormSubmit write FOnFormSubmit;
  end;

implementation

function ConvDosToHTML(const Name: ThtString): ThtString; forward;

{----------------TbrFrame.CreateIt}

//-- BG ---------------------------------------------------------- 03.01.2010 --
function TbrFrame.ExpandSourceName(Base, Path: ThtString; S: ThtString): ThtString;
begin
  S := ConvDosToHTML(S);
  if Pos(':/', S) <> 0 then
    URLBase := URLSubs.GetURLBase(S) {get new base}
  else if Base <> '' then
  begin
    S := CombineURL(Base, S);
    URLBase := Base;
  end
  else
  begin
    if LOwner is TbrFrameSet then
      URLBase := (LOwner as TbrFrameSet).URLBase
    else
      URLBase := (LOwner as TbrSubFrameSet).URLBase;
    S := CombineURL(URLBase, S);
  end;
  Result := S
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TbrFrame.FrameSet: TbrSubFrameSet;
begin
  Result := TbrSubFrameSet(inherited FrameSet);
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TbrFrame.GetSubFrameSetClass: TSubFrameSetClass;
begin
  Result := TbrSubFrameSet;
end;

//-- BG ---------------------------------------------------------- 03.01.2010 --
function TbrFrame.MasterSet: TbrFrameSet;
begin
  Result := TbrFrameSet(inherited MasterSet);
end;

procedure TbrFrame.RefreshEvent(Sender: TObject; Delay: integer; const URL: ThtString);
var
  Ext: ThtString;
begin
  if not (fvMetaRefresh in MasterSet.FrameViewer.fvOptions) then
    Exit;
  Ext := Lowercase(GetURLExtension(URL));
  if (Ext = 'exe') or (Ext = 'zip') then
    Exit;
  if URL = '' then
    NextFile := Source
  else if not IsFullURL(URL) then
    NextFile := CombineURL(URLBase, URL) //URLBase + URL
  else
    NextFile := URL;
  if not Assigned(RefreshTimer) then
    RefreshTimer := TTimer.Create(Self);
  RefreshTimer.OnTimer := RefreshTimerTimer;
  RefreshTimer.Interval := Delay * 1000;
  RefreshTimer.Enabled := True;
end;

procedure TbrFrame.RefreshTimerTimer(Sender: TObject);
var
  S, D: ThtString;
begin
  RefreshTimer.Enabled := False;
  if Unloaded then
    Exit;
  if not IsFullUrl(NextFile) then
    NextFile := CombineURL(UrlBase, NextFile);

  SplitDest(NextFile, S, D);
  if (MasterSet.Viewers.Count = 1) then {load a new FrameSet}
    MasterSet.FrameViewer.LoadURLInternal(S, D, '', '', '', True, True)
  else
    frLoadFromBrzFile(S, D, '', '', '', True, True, True);
end;

procedure TbrFrame.CreateViewer;
begin
  inherited CreateViewer;
  Viewer.OnExpandName := UrlExpandName;
end;

{----------------TbrFrame.LoadFiles}

procedure TbrFrame.LoadFiles;
var
  Item: TFrameBaseOpener;
  I: integer;
  Upper, Lower: boolean;
  NewURL: ThtString;
  Doc: TBuffer;
begin
  if (Source <> '') and (MasterSet.NestLevel < 4) then
  begin
    if not Assigned(TheStream) then
    begin
      NewURL := '';
      MasterSet.FrameViewer.PostRequest(Self, True, Source, '', '', '', False, NewURL, TheStreamType, TheStream);
      if NewURL <> '' then
        Source := NewURL;
    end;
    URLBase := URLSubs.GetURLBase(Source);
    Inc(MasterSet.NestLevel);
    try
      try
        if TheStream <> nil then
        begin
          TheStream.Position := 0;
          Doc := TBuffer.Create(TheStream, Source);
        end
        else
          Doc := nil;
        try
          if (TheStreamType = HTMLType) and MasterSet.FrameViewer.IsFrame(Doc) then
          begin
            FFrameSet := TbrSubFrameSet.CreateIt(Self, MasterSet);
            FrameSet.Align := alClient;
            FrameSet.Visible := False;
            InsertControl(FrameSet);
            FrameSet.SendToBack;
            FrameSet.Visible := True;
            MasterSet.FrameViewer.ParseFrame(FrameSet, Doc, Source, FrameSet.HandleMeta);
            Self.BevelOuter := bvNone;
            frBumpHistory1(Source, 0);
            with FrameSet do
            begin
              for I := 0 to List.Count - 1 do
              begin
                Item := TFrameBaseOpener(List.Items[I]);
                Item.LoadFiles;
              end;
              CheckNoresize(Lower, Upper);
              if FRefreshDelay > 0 then
                SetRefreshTimer;
            end;
          end
          else
          begin
            CreateViewer;
            Viewer.Base := MasterSet.FBase;
            Viewer.LoadFromStream(TheStream, Source, TheStreamType);
            Viewer.PositionTo(Destination);
            frBumpHistory1(Source, Viewer.Position);
          end;
        finally
          Doc.Free;
        end;
      except
        if not Assigned(Viewer) then
          CreateViewer;
        FreeAndNil(FFrameSet);
        Viewer.LoadFromString('<p><img src="qw%&.bmp" alt="Error"> Can''t load ' + Source); {load an error message}
      end;
    finally
      Dec(MasterSet.NestLevel);
    end;
  end
  else
  begin {so blank area will perform like the TFrameBrowser}
    OnMouseDown := MasterSet.FrameViewer.OnMouseDown;
    OnMouseMove := MasterSet.FrameViewer.OnMouseMove;
    OnMouseUp := MasterSet.FrameViewer.OnMouseUp;
    OnMouseWheel := MasterSet.FrameViewer.OnMouseWheel;
    OnMouseWheelUp := MasterSet.FrameViewer.OnMouseWheelUp;
    OnMouseWheelDown := MasterSet.FrameViewer.OnMouseWheelDown;
  end;
end;

{----------------TbrFrame.ReloadFiles}

procedure TbrFrame.ReloadFiles(APosition: LongInt);
var
  Item: TFrameBaseOpener;
  I: integer;
  Upper, Lower: boolean;
  Dummy: ThtString;

  procedure DoError;
  begin
    Viewer.LoadFromString('<p><img src="qw%&.bmp" alt="Error"> Can''t load ' + Source); {load an error message}
  end;

begin
  if Source <> '' then
    if FrameSet <> nil then
    begin
      with FrameSet do
      begin
        for I := 0 to List.Count - 1 do
        begin
          Item := TFrameBaseOpener(List.Items[I]);
          Item.ReloadFiles(APosition);
        end;
        CheckNoresize(Lower, Upper);
      end;
    end
    else if Assigned(Viewer) then
    begin
      Viewer.Base := MasterSet.FBase; {only effective if no Base to be read}
      try
        MasterSet.FrameViewer.PostRequest(Self, True, Source, '', '', '', True, Dummy, TheStreamType, TheStream);
        Viewer.LoadFromStream(TheStream, Source, TheStreamType);
        if APosition < 0 then
          Viewer.Position := ViewerPosition
        else
          Viewer.Position := APosition; {its History Position}
        Viewer.FormData := ViewerFormData;
        ViewerFormData.Free;
        ViewerFormData := nil;
      except
        DoError;
      end;
    end;
  Unloaded := False;
end;

{----------------TbrFrame.frLoadFromBrzFile}

procedure TbrFrame.frLoadFromBrzFile(const URL, Dest, Query, EncType, Referer: ThtString; Bump, IsGet, Reload: boolean);
{URL is full URL here, has been seperated from Destination}
var
  OldPos: LongInt;
  HS, S, S1, OldTitle, OldName, OldBase: ThtString;
  OldFormData: TFreeList;
  SameName: boolean;
  OldViewer: ThtmlViewer;
  OldFrameSet: TbrSubFrameSet;
  Upper, Lower, FrameFile: boolean;
  Item: TFrameBase;
  I: integer;
  Doc: TBuffer;
begin
  if Assigned(RefreshTimer) then
    RefreshTimer.Enabled := False;
  OldName := Source;
  OldBase := URLBase;
  S := URL;
  if S = '' then
    S := OldName
  else
    URLBase := URLSubs.GetURLBase(S); {get new base}
  HS := S;
  SameName := CompareText(S, OldName) = 0;
{if SameName, will not have to reload anything unless Reload set}

  if not SameName or Reload then
  begin
    if Assigned(Viewer) and Assigned(MasterSet.FrameViewer.OnViewerClear) then
      MasterSet.FrameViewer.OnViewerClear(Viewer);
    MasterSet.FrameViewer.PostRequest(Self, IsGet, S, Query, EncType, Referer, Reload, S1, TheStreamType, TheStream);
    if S1 <> '' then
    begin
      S := S1;
      URLBase := URLSubs.GetURLBase(S);
    end;
    Source := S;
  end;

  try
    //TheString := StreamToString(TheStream);
    if TheStream <> nil then
    begin
      TheStream.Position := 0;
      Doc := TBuffer.Create(TheStream)
    end
    else
      Doc := nil;
    try
      if not SameName then
        FrameFile := (TheStreamType = HTMLType) and MasterSet.FrameViewer.IsFrame(Doc)
      else
        FrameFile := not Assigned(Viewer);
      if SameName and not Reload then
        if Assigned(Viewer) then
        begin
          OldPos := Viewer.Position;
          Viewer.PositionTo(Dest);
          MasterSet.FrameViewer.AddVisitedLink(URL + Dest);
          if Bump and (Viewer.Position <> OldPos) then
          {Viewer to Viewer}
            frBumpHistory(HS, Viewer.Position, OldPos, nil);
        end
        else
        begin
          with FrameSet do
            for I := 0 to List.Count - 1 do
            begin
              Item := TFrameBase(List.Items[I]);
              if (Item is TbrFrame) then
                with TbrFrame(Item) do
                  if CompareText(Source, OrigSource) <> 0 then
                    frLoadFromBrzFile(OrigSource, '', '', '', '', True, True, False);
            end;
          Exit;
        end
      else if Assigned(Viewer) and not FrameFile then {not samename or samename and reload}
      begin {Viewer already assigned and it's not a Frame file}
        OldPos := Viewer.Position;
        OldTitle := Viewer.DocumentTitle;
        if Bump and not SameName and (MasterSet.Viewers.Count > 1) then
          OldFormData := Viewer.FormData
        else
          OldFormData := nil;
        try
          Viewer.Base := MasterSet.FBase;
          Viewer.LoadFromStream(TheStream, Source, TheStreamType);
          Viewer.PositionTo(Dest);
          MasterSet.FrameViewer.AddVisitedLink(URL + Dest);
          if not samename then
          begin {don't bump history on a forced reload}
            if MasterSet.Viewers.Count > 1 then
            begin
              if Bump then
               {Viewer to Viewer}
                frBumpHistory(HS, Viewer.Position, OldPos, OldFormData)
              else
                OldFormData.Free;
            end
            else if (MasterSet.Viewers.Count = 1) and Bump then
            {a single viewer situation, bump the history here}
              with MasterSet do
              begin
                FCurrentFile := Source;
                FTitle := Viewer.DocumentTitle;
                FBase := Viewer.Base;
                FBaseTarget := Viewer.BaseTarget;
                FrameViewer.BumpHistory1(OldName, OldTitle, OldPos, HTMLType);
              end;
          end;
        except
          OldFormData.Free;
          raise;
        end;
      end
      else
      begin {Viewer is not assigned or it is a Frame File}
      {keep the old viewer or frameset around (free later) to minimize blink}
        OldViewer := Viewer;       FViewer := nil;
        OldFrameSet := FrameSet; FFrameSet := nil;
        if OldFrameSet <> nil then
          OldFrameSet.ClearFrameNames;
        if FrameFile then
        begin {it's a frame file}
          FFrameSet := TbrSubFrameSet.CreateIt(Self, MasterSet);
          FrameSet.URLBase := URLBase;
          FrameSet.Align := alClient;
          FrameSet.Visible := False;
          InsertControl(FrameSet);
          FrameSet.SendToBack; {to prevent blink}
          FrameSet.Visible := True;
          MasterSet.FrameViewer.ParseFrame(FrameSet, Doc, Source, FrameSet.HandleMeta);
          MasterSet.FrameViewer.AddVisitedLink(URL);
          Self.BevelOuter := bvNone;
          with FrameSet do
          begin
            for I := 0 to List.Count - 1 do
              TFrameBaseOpener(List.Items[I]).LoadFiles;
            CheckNoresize(Lower, Upper);
            if FRefreshDelay > 0 then
              SetRefreshTimer;
          end;
          if Assigned(OldViewer) then
            frBumpHistory(HS, 0, OldViewer.Position, OldViewer.FormData)
          else
            frBumpHistory(S, 0, 0, nil);
        end
        else
        begin {not a frame file but needs a viewer}
          CreateViewer;
          Viewer.Base := MasterSet.FBase;
          Viewer.LoadFromStream(TheStream, Source, TheStreamType);
          Viewer.PositionTo(Dest);
          MasterSet.FrameViewer.AddVisitedLink(URL + Dest);
        {FrameSet to Viewer}
          frBumpHistory(HS, Viewer.Position, 0, nil);
        end;
        if FrameSet <> nil then
          with FrameSet do
          begin
            with ClientRect do
              InitializeDimensions(Left, Top, Right - Left, Bottom - Top);
            CalcSizes(nil);
          end;
        if Assigned(Viewer) then
        begin
          if MasterSet.BorderSize = 0 then
            BevelOuter := bvNone
          else
          begin
            BevelOuter := bvLowered;
            BevelWidth := MasterSet.BorderSize;
          end;
          if (Dest <> '') then
            Viewer.PositionTo(Dest);
        end;
        if Assigned(OldViewer) then
        begin
          MasterSet.Viewers.Remove(OldViewer);
          if MasterSet.FActive = OldViewer then
            MasterSet.FActive := nil;
          OldViewer.Free;
        end
        else if Assigned(OldFrameSet) then
        begin
          OldFrameSet.UnloadFiles;
          OldFrameSet.Visible := False;
        end;
        Invalidate; //RePaint;
      end;
    finally
      Doc.Free;
    end;
  except
    Source := OldName;
    URLBase := OldBase;
    raise;
  end;
end;

procedure TbrFrame.frLoadFromFile(const FName, Dest: ThtString; Bump, Reload: Boolean);
begin
  frLoadFromBrzFile(FName, Dest, '', '', Viewer.CurrentFile, Bump, True, Reload);
end;

function ConvDosToHTML(const Name: ThtString): ThtString;
{if Name is a Dos filename, convert it to HTML.  Add the file:// if it is
 a full pathe filename}
begin
  Result := Name;
  if Pos('\', Result) > 0 then
  begin
    Result := DosToHTML(Result);
    if (Pos('|', Result) > 0) then {was something like c:\....}
      Result := 'file:///' + Result;
  end;
end;

{----------------TbrFrame.URLExpandName}

procedure TbrFrame.URLExpandName(Sender: TObject; const SRC: ThtString; var Rslt: ThtString);
var
  S: ThtString;
  Viewer: ThtmlViewer;
begin
  S := ConvDosToHTML(SRC);
  if not IsFullUrl(S) then
  begin
    Viewer := Sender as ThtmlViewer;
    if Viewer.Base <> '' then
      Rslt := CombineURL(ConvDosToHTML(Viewer.Base), S)
    else
      Rslt := CombineURL(UrlBase, S);
  end
  else
    Rslt := S;
end;

{----------------TbrSubFrameSet.CreateIt}

constructor TbrSubFrameSet.CreateIt(AOwner: TComponent; Master: TFrameSetBase);
begin
  inherited CreateIt(AOwner, Master);
  if (AOwner is TbrFrame) then
    URLBase := TbrFrame(AOwner).URLBase
  else if (AOwner is TbrFrameSet) then
    URLBase := TbrFrameSet(AOwner).URLBase
  else if (AOwner is TbrSubFrameSet) then
    URLBase := TbrSubFrameSet(AOwner).URLBase;
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TbrSubFrameSet.GetFrameClass: TViewerFrameClass;
begin
  Result := TbrFrame;
end;

{----------------TbrSubFrameSet.RefreshTimerTimer}

procedure TbrSubFrameSet.RefreshTimerTimer(Sender: Tobject);
var
  S, D: ThtString;
begin
  RefreshTimer.Enabled := False;
  if Unloaded then
    Exit;
  if Owner is TbrFrame then
  begin
    SplitDest(NextFile, S, D);
    TbrFrame(Owner).frLoadFromBrzFile(S, D, '', '', '', True, True, True)
  end;
end;

//-- BG ---------------------------------------------------------- 03.01.2010 --
function TbrFrameSet.FrameViewer: TFrameBrowser;
begin
  Result := TFrameBrowser(inherited FrameViewer);
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TbrFrameSet.GetFrameClass: TViewerFrameClass;
begin
  Result := TbrFrame;
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TbrFrameSet.MasterSet: TbrFrameSet;
begin
  Result := TbrFrameSet(inherited MasterSet);
end;

procedure TbrFrameSet.RefreshTimerTimer(Sender: Tobject);
var
  S, D: ThtString;
begin
  RefreshTimer.Enabled := False;
  if Self = MasterSet.FrameViewer.CurFrameSet then
  begin
    SplitDest(NextFile, S, D);
    FrameViewer.LoadURLInternal(S, D, '', '', '', True, True);
  end;
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TbrFrameSet.RequestEvent: Boolean;
begin
  Result := True;
end;

{----------------TbrFrameSet.LoadFromBrzFile}

procedure TbrFrameSet.LoadFromBrzFile(Stream: TMemoryStream; StreamType: ThtmlFileType;
  const URL, Dest: ThtString);
var
  I: integer;
  Frame: TbrFrame;
  Lower, Upper: boolean;
  Doc: TBuffer;
begin
  Clear;
  NestLevel := 0;
  FCurrentFile := URL;
  Stream.Position := 0;
  Doc := TBuffer.Create(Stream, Url);
  try
    if (StreamType = HTMLType) and MasterSet.FrameViewer.IsFrame(Doc) then
    begin {it's a Frameset html file}
      MasterSet.FrameViewer.ParseFrame(Self, Doc, Url, HandleMeta);
      for I := 0 to List.Count - 1 do
        TFrameBaseOpener(List.Items[I]).LoadFiles;
      CalcSizes(Self);
      CheckNoresize(Lower, Upper);
      if FRefreshDelay > 0 then
        SetRefreshTimer;
    end
    else
    begin {it's a non frame file}
      Frame := TbrFrame(AddFrame(nil, ''));
      Frame.Source := URL;
      Frame.TheStream := Stream;
      Frame.TheStreamType := StreamType;
      Frame.Destination := Dest;
      Parsed('', '', '');
      CalcSizes(Self);
      Frame.LoadFiles;
      FTitle := Frame.Viewer.DocumentTitle;
      FBase := Frame.Viewer.Base;
      FBaseTarget := Frame.Viewer.BaseTarget;
    end;
  finally
    Doc.Free;
  end;
end;

{----------------TFrameBrowser.Create}

constructor TFrameBrowser.Create(AOwner: TComponent);
begin
  inherited;
  FEncodePostArgs := True;
end;

//-- BG ---------------------------------------------------------- 24.09.2010 --
procedure TFrameBrowser.LoadFromFile(const Name: ThtString);
begin
  LoadUrl('file://' + DosToHTML(Name));
end;

{----------------TFrameBrowser.LoadURL}

procedure TFrameBrowser.LoadURL(const URL: ThtString);
var
  S, D: ThtString;
begin
  if not Processing then
  begin
    SplitDest(Normalize(URL), S, D);
    LoadURLInternal(S, D, '', '', '', True, False);
  end;
end;

{----------------TFrameBrowser.GetPostQuery}

procedure TFrameBrowser.GetPostQuery(const URL, Query, EncType: ThtString; IsGet: boolean);
var
  S, D: ThtString;
begin
  if not Processing then
  begin
    SplitDest(Normalize(URL), S, D);
    LoadURLInternal(S, D, Query, EncType, '', IsGet, True);
  end;
end;

//-- BG ---------------------------------------------------------- 05.01.2010 --
function TFrameBrowser.GetFrameSetClass: TFrameSetClass;
begin
  Result := TbrFrameSet;
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TFrameBrowser.GetSubFrameSetClass: TSubFrameSetClass;
begin
  Result := TbrSubFrameSet;
end;

{----------------TFrameBrowser.LoadURLInternal}

procedure TFrameBrowser.LoadURLInternal(const URL, Dest, Query, EncType, Referer: ThtString;
  IsGet, Reload: boolean);
var
  OldFrameSet: TbrFrameSet;
  OldFile, S, S1: ThtString;
  OldPos: LongInt;
  Tmp: TObject;
  SameName: boolean;
  Stream: TMemoryStream;
  StreamType: ThtmlFileType;
  I: integer;
begin
  AssertCanPostRequest(URL);
  BeginProcessing;
  IOResult; {remove any pending file errors}
  S := URL;
  try
    OldFile := CurbrFrameSet.FCurrentFile;
    ProcessList.Clear;
    if Assigned(OnSoundRequest) then
      OnSoundRequest(Self, '', 0, True);
    SameName := CompareText(OldFile, S) = 0;
    if not SameName then
    begin
      if Assigned(OnViewerClear) then
        for I := 0 to CurbrFrameSet.Viewers.Count - 1 do
          OnViewerClear(CurbrFrameSet.Viewers[I]);
      OldFrameSet := CurbrFrameSet;
      FCurFrameSet := TbrFrameSet.Create(Self);
      CurFrameSet.Align := alClient;
      CurFrameSet.visible := False;
      InsertControl(CurbrFrameSet);
      CurFrameSet.SendToBack;
      CurFrameSet.Visible := True;

      try
        S1 := '';
        PostRequest(Self, IsGet, S, Query, EncType, Referer, Reload, S1, StreamType, Stream);
        if not Assigned(Stream) then
          raise EhtLoadError.CreateFmt('Can''t locate ''%s''.', [S]);
        if S1 <> '' then
          S := S1;

        if Pos(':', S) <> 0 then
          CurbrFrameSet.URLBase := URLSubs.GetURLBase(S)
        else
        begin
          CurbrFrameSet.URLBase := OldFrameSet.URLBase;
          S := CombineURL(CurbrFrameSet.URLBase, S);
        end;

        (CurbrFrameSet as TbrFrameSet).LoadFromBrzFile(Stream, StreamType, S, Dest);
      except
        RemoveControl(CurbrFrameSet);
        CurbrFrameSet.Free;
        FCurFrameSet := OldFrameSet;
        raise;
      end;

      OldPos := 0;
      if (OldFrameSet.Viewers.Count = 1) then
      begin
        Tmp := OldFrameSet.Viewers[0];
        if Tmp is ThtmlViewer then
          OldPos := ThtmlViewer(Tmp).Position;
      end;
      OldFrameSet.UnloadFiles;
      CurbrFrameSet.Visible := True;
      CurbrFrameSet.BringToFront;
//      if Visible then
//      begin
//        SendMessage(Handle, wm_SetRedraw, 0, 0);
//        try
//          CurbrFrameSet.BringToFront;
//        finally
//          SendMessage(Handle, wm_SetRedraw, 1, 0);
//          Repaint;
//        end;
//        CurbrFrameSet.Repaint;
//      end;

      BumpHistory(OldFrameSet, OldPos);
    end
    else
    begin {Same name}
      OldPos := 0;
      if (CurbrFrameSet.Viewers.Count = 1) then
      begin
        Tmp := CurbrFrameSet.Viewers[0];
        if Tmp is ThtmlViewer then
          OldPos := ThtmlViewer(Tmp).Position;
      end;
      PostRequest(Self, IsGet, S, Query, EncType, Referer, Reload, S1, StreamType, Stream);
      if not Assigned(Stream) then
        raise EhtLoadError.CreateFmt('Can''t locate cache file ''%s''.', [S]);

      if S1 <> '' then
      begin
        S := S1;
        if Pos(':', S) <> 0 then
          CurbrFrameSet.URLBase := URLSubs.GetURLBase(S);
      end;

      (CurbrFrameSet as TbrFrameSet).LoadFromBrzFile(Stream, StreamType, S, Dest);
      BumpHistory2(OldPos); {not executed if exception occurs}
    end;
    AddVisitedLink(URL+Dest);
  finally
    EndProcessing;
  end;
end;

//-- BG ---------------------------------------------------------- 23.09.2010 --
// concentrate all FOnGetPostRequet* calls here:
procedure TFrameBrowser.PostRequest(Sender: TObject; IsGet: boolean; const Source, Query, EncType,
  Referer: ThtString; Reload: boolean; out NewURL: ThtString; out DocType: ThtmlFileType;
  out Stream: TMemoryStream);
begin
  NewURL := '';
  DocType := OtherType;
  Stream := nil;
  if Assigned(FOnGetPostRequestEx) then
    FOnGetPostRequestEx(Self, IsGet, Source, Query, EncType, Referer, Reload, NewURL, DocType, Stream)
  else if Assigned(FOnGetPostRequest) then
    FOnGetPostRequest(Self, IsGet, Source, Query, Reload, NewURL, DocType, Stream)
  else if Copy(Source, 1, 7) = 'file://' then
  begin
    DocType := getFileType(Source);
    if DocType <> OtherType then
    begin
      Stream := TMemoryStream.Create;
      Stream.LoadFromFile(HTMLToDos(Source));
    end;
  end;
end;

{----------------TFrameBrowser.HotSpotClick}

procedure TFrameBrowser.HotSpotClick(Sender: TObject; const AnURL: ThtString; var Handled: boolean);
var
  I: integer;
  Viewer: ThtmlViewer;
  FrameTarget: TFrameBase;
  S, Dest, FullUrl: ThtString;
begin
  Handled := True;
  if Processing then
    Exit;

  Viewer := (Sender as ThtmlViewer);
  FURL := AnURL;
  FTarget := GetActiveTarget;
  FLinkAttributes.Text := Viewer.LinkAttributes.Text;
  FLinkText := Viewer.LinkText;

  SplitDest(AnUrl, S, Dest);
  S := ConvDosToHTML(S);
  if S = '' then
    FullUrl := (Viewer.FrameOwner as TbrFrame).Source
  else if IsFullURL(S) then
    FullUrl := S
  else if Viewer.Base <> '' then
    FullUrl := CombineURL(ConvDosToHTML(Viewer.Base), S)
  else
    FullUrl := CombineURL((Viewer.FrameOwner as TbrFrame).URLBase, S);
  FullUrl := Normalize(FullUrl);  // ANGUS
  if not HotSpotClickHandled(FullUrl + Dest) then
  begin
    Handled := True;
    if (FTarget = '') or (CompareText(FTarget, '_self') = 0) then {no target or _self target}
    begin
      FrameTarget := Viewer.FrameOwner as TbrFrame;
      if not Assigned(FrameTarget) then
        Exit;
    end
    else if CurbrFrameSet.FrameNames.Find(FTarget, I) then
      FrameTarget := (CurbrFrameSet.FrameNames.Objects[I] as TbrFrame)
    else if CompareText(FTarget, '_top') = 0 then
      FrameTarget := CurbrFrameSet
    else if CompareText(FTarget, '_parent') = 0 then
    begin
      FrameTarget := (Viewer.FrameOwner as TbrFrame).Owner as TFrameBase;
      while Assigned(FrameTarget) and not (FrameTarget is TbrFrame)
        and not (FrameTarget is TbrFrameSet) do
        FrameTarget := FrameTarget.Owner as TFrameBase;
    end
    else
    begin
      if Assigned(OnBlankWindowRequest) then
      begin
        AddVisitedLink(FullUrl + Dest);
        CheckVisitedLinks;
        OnBlankWindowRequest(Self, FTarget, FullUrl + Dest);
        Handled := True;
      end
      else
        Handled := FTarget <> ''; {true if can't find target window}
      Exit;
    end;

    BeginProcessing;
    if (FrameTarget is TbrFrame) and (CurbrFrameSet.Viewers.Count = 1) and (S <> '')
      and (CompareText(S, CurbrFrameSet.FCurrentFile) <> 0) then
      FrameTarget := CurbrFrameSet; {force a new FrameSet on name change}
    try
      if FrameTarget is TbrFrame then
        TbrFrame(FrameTarget).frLoadFromBrzFile(FullUrl, Dest, '', '', Viewer.CurrentFile, True, True, False)
      else if FrameTarget is TbrFrameSet then
        LoadURLInternal(FullUrl, Dest, '', '', Viewer.CurrentFile, True, False);
      CheckVisitedLinks;
    finally
      EndProcessing;
    end;
  end;
end;

{----------------TFrameBrowser.HotSpotCovered}

procedure TFrameBrowser.HotSpotCovered(Sender: TObject; const SRC: ThtString);
var
  S, Dest, FullUrl: ThtString;
  Viewer: ThtmlViewer;
begin
  if Assigned(OnHotSpotTargetCovered) then
  begin
    Viewer := Sender as ThtmlViewer;
    SplitDest(SRC, S, Dest);
    S := ConvDosToHTML(S); {convert DOS names}
    if IsFullURL(S) or (Src = '') then
      FullUrl := S
    else
    begin
      if Viewer.Base <> '' then
        FullUrl := CombineURL(ConvDosToHTML(Viewer.Base), S)
      else
        FullUrl := CombineURL((Viewer.FrameOwner as TbrFrame).URLBase, S);
    end;
    FLinkText := Viewer.LinkText;
    FLinkAttributes.Text := Viewer.LinkAttributes.Text;
    OnHotSpotTargetCovered(Sender, (Sender as ThtmlViewer).Target, FullUrl + Dest);
  end;
end;

//-- BG ---------------------------------------------------------- 04.01.2010 --
function TFrameBrowser.CurbrFrameSet: TbrFrameSet;
begin
  Result := TbrFrameSet(CurFrameSet);
end;

{----------------TFrameBrowser.DoFormSubmitEvent}

procedure TFrameBrowser.DoFormSubmitEvent(Sender: TObject; const Action, Target, EncType,
  Method: ThtString; Results: ThtStringList);
var
  S, Dest, Query: ThtString;
  FrameTarget: TFrameBase;
  I: integer;
  Viewer: ThtmlViewer;
  UserHandled, IsGet: boolean;

  function AssembleQuery: ThtString;
  var
    S1: ThtString;
    I, J: integer;

    function Encode(const S: ThtString): ThtString;
    var
      Ch: ThtChar;
      I: integer;
    begin {convert odd chars into %xx -- does not handle the '=' sign yet}
      Result := '';
      for I := 1 to Length(S) do
      begin
        Ch := S[I];
        case Ch of
          ' ':
            htAppendChr(Result, '+');

          'a'..'z',
          'A'..'Z',
          '0'..'9',
          '=', '_',
          '-', '.',
          '*', '@':
            htAppendChr(Result, Ch);
        else
          htAppendChr(Result, '%');
          htAppendStr(Result, IntToHex(ord(Ch), 2));
        end;
      end;
    end;

  begin
    Result := '';
    for I := 0 to Results.Count - 1 do
    begin
      if FEncodePostArgs then
      begin {form a ThtString from the TStringList using '+' for spaces and '&' for separaters}
        S1 := Encode(Results[I]);
        J := Pos(' ', S1);
        while J > 0 do
        begin
          S1[J] := '+';
          J := Pos(' ', S1);
        end;
      end
      else
        S1 := Trim(Results[I]); {No encoding done}
      if I <> 0 then
        Result := Result + '&';
      Result := Result + S1;
    end;
    Results.Free;
  end;

begin
  if InFormSubmit then
    Exit;
  InFormSubmit := True;
  try
  {see if the application wants to handle this event}
    UserHandled := false;
    Viewer := (Sender as ThtmlViewer);
    if Assigned(FOnFormSubmit) then
      FOnFormSubmit(Self, Viewer, Action, Target, EncType, Method, Results, UserHandled);
    if not UserHandled then
    begin
      Query := AssembleQuery;

      if (Target = '') or (CompareText(Target, '_self') = 0) then {no target or _self target}
        FrameTarget := Viewer.FrameOwner as TbrFrame
      else if CurbrFrameSet.FrameNames.Find(Target, I) then
        FrameTarget := (CurbrFrameSet.FrameNames.Objects[I] as TbrFrame)
      else if CompareText(Target, '_top') = 0 then
        FrameTarget := CurbrFrameSet
      else if CompareText(Target, '_parent') = 0 then
      begin
        FrameTarget := (Viewer.FrameOwner as TbrFrame).Owner as TFrameBase;
        while Assigned(FrameTarget) and not (FrameTarget is TbrFrame)
          and not (FrameTarget is TbrFrameSet) do
          FrameTarget := FrameTarget.Owner as TFrameBase;
      end
      else
      begin
        if Assigned(OnBlankWindowRequest) then
          OnBlankWindowRequest(Self, Target, Action + '?' + Query);
        Exit;
      end;

      S := Action;
      I := Pos('#', S);
      if I >= 1 then
      begin
        Dest := System.Copy(S, I, Length(S) - I + 1); {local destination}
        S := System.Copy(S, 1, I - 1); {the file name}
      end
      else
        Dest := ''; {no local destination}

      BeginProcessing;
      try
        if (FrameTarget is TbrFrame) and (CurbrFrameSet.Viewers.Count = 1) and (S <> '')
          and (CompareText(S, CurbrFrameSet.FCurrentFile) <> 0) then
          FrameTarget := CurbrFrameSet; {force a new FrameSet on name change}
        if S = '' then
          S := (Viewer.FrameOwner as TbrFrame).Source
        else if not IsFullURL(S) then
          S := CombineURL((Viewer.FrameOwner as TbrFrame).URLBase, S);
        IsGet := CompareText(Method, 'get') = 0;
        if FrameTarget is TbrFrame then
          TbrFrame(FrameTarget).frLoadFromBrzFile(S, Dest, Query, EncType, Viewer.CurrentFile, True, IsGet, True)
        else if FrameTarget is TbrFrameSet then
          LoadURLInternal(S, Dest, Query, EncType, Viewer.CurrentFile, IsGet, True);
      finally
        EndProcessing;
      end;
    end;
  finally
    InFormSubmit := False;
  end;
end;

procedure TFrameBrowser.DoURLRequest(Sender: TObject; const SRC: ThtString; var Stream: TMemoryStream);
var
  NewURL: ThtString;
  DocType: ThtmlFileType;
begin
  PostRequest(Sender, True, SRC, '', '', '', False, NewURL, DocType, Stream);
end;

{----------------TFrameBrowser.GetViewerUrlBase}

function TFrameBrowser.GetViewerUrlBase(Viewer: ThtmlViewer): ThtString;
var
  Frame: TbrFrame;
begin
  try
    Frame := Viewer.FrameOwner as TbrFrame;
    Result := Frame.UrlBase;
  except
    Result := '';
  end;
end;

//-- BG ---------------------------------------------------------- 23.03.2012 --
procedure TFrameBrowser.AssertCanPostRequest(const URL: ThtString);
begin
  if not SameText(Copy(URL, 1, 7), 'file://') then
    if not Assigned(FOnGetPostRequest) and not Assigned(FOnGetPostRequestEx) then
      raise Exception.Create('Don''t know how to load an URL. Neither OnGetPostRequest nor OnGetPostRequestEx event defined.');
end;

{----------------TFrameBrowser.CheckVisitedLinks}

procedure TFrameBrowser.CheckVisitedLinks;
var
  I, J, K: integer;
  S, S1: ThtString;
  Viewer: ThtmlViewer;
begin
  if VisitedMaxCount = 0 then
    Exit;
  for K := 0 to CurbrFrameSet.Viewers.Count - 1 do
  begin
    Viewer := ThtmlViewer(CurbrFrameSet.Viewers[K]);
    for I := 0 to Visited.Count - 1 do
    begin
      S := Visited[I];
      for J := 0 to Viewer.LinkList.Count - 1 do
        with TFontObj(Viewer.LinkList[J]) do
        begin
          if Url <> '' then
          begin
            if IsFullURL(Url) then
              S1 := Url
            else if Url[1] = '#' then
              S1 := TbrFrame(Viewer.FrameOwner).Source + Url
            else
              S1 := CombineURL(TbrFrame(Viewer.FrameOwner).UrlBase, Url);
            if CompareText(S, S1) = 0 then
              Visited := True;
          end;
        end;
    end;
    Viewer.Invalidate;
  end;
end;

end.
