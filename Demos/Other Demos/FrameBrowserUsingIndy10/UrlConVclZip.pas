{
Version   11.7
Copyright (c) 2016 by HtmlViewer Team

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

Note that the source modules HTMLGIF1.PAS, DITHERUNIT.PAS
are covered by separate copyright notices located in those modules.
}
unit UrlConVclZip;
{
This unit implements a ThtConnector/ThtConnection pair for reading documents
from a ZIP file. The core TZipConnection code hasn't been tested for a long time
but it is worth enough to be kept in the project. Did it ever work? What about
directories inside the ZIP file...

Protocol syntax:                   "zip://zipname/filetoextract"
The zip's full path is needed, as: "zip://c:\dir1\subdir\htmlfiles.zip/demo.htm"
or:                                "zip://c|/dir1/subdir/htmlfiles.zip/demo.htm"

The core TZipConnection code has been extracted from the former UrlConId10.PAS.

Thanks to the Indy Pit Crew for updating the former
UrlConId10.pas/FBUnitId10.pas to Indy 10
from Indy 9 in UrlConId9.pas/FBUnitId9.pas.

UrlConId9.PAS was written as UrlConId.PAS by Yves Urbain.
}
// This is the original copyright notice of UrlConId.PAS:
{*********************************************************}
{*                     UrlConId.PAS                      *}
{*                Copyright (c) 1999 by                  *}
{*                   Metaphor SPRL                       *}
{*                 All rights reserved.                  *}
{*                Written by Yves Urbain                 *}
{*********************************************************}

{.$define IncludeZip}

{$include htmlcons.inc}

interface

{$ifdef IncludeZip}
uses
  Classes, SysUtils,
  VCLUnZIp, kpZipObj,
  UrlSubs, UrlConn;

type
  TZipConnection = class(ThtConnection)
  private
    UnZipper: TVCLUnzip;
  public
    destructor Destroy; override;
    procedure Get(Doc: ThtUrlDoc); override;
  end;

  TZipConnector = class(ThtConnector)
  protected
    class function GetDefaultProtocols: string; override;
    class function GetVersion: string; override;
  public
    function CreateConnection(const Protocol: String): ThtConnection; override;
  end;
{$endif}

implementation

{$ifdef IncludeZip}

type
  EZipFileError = class(Exception);

{----------------TZipConnection.Destroy}
destructor TZipConnection.Destroy;
begin
  If Unzipper <> nil then
     Unzipper.free;
  inherited;
end;

procedure TZipConnection.Get(Doc: ThtUrlDoc);
var
  num, I: Integer;
  TheFile, Host, Ext : String;
begin
  if Unzipper = nil then
    Unzipper := TVCLUnzip.Create(nil);
  TheFile := Doc.URL;

  {remove any query string as it's not certain how to respond to a Form
   submit with a zip: protocol.  The user can add a response if desired.}
  I := Pos('?', TheFile);
  if I > 0 then
    TheFile := Copy(TheFile, 1, I-1);

  TheFile := GetURLFilenameAndExt(TheFile);
  Host := GetBase(TheFile);
  Delete(Host, 1, 6);     {remove zip://}
  Delete(Host, Length(Host), 1);  {remove trailing '/'}
  Host := HTMLToDos(Host);

  Ext := GetURLExtension(TheFile);
  Doc.DocType := FileExt2DocType(Ext);

  if Unzipper.ZipName <> Host then
    Unzipper.ZipName := Host;              { set the zip filename}
  try
    { Extract files, return value is the number of files actually unzipped}
    num := Unzipper.UnZipToStream( Doc.Stream, TheFile );
  except
    on E: Exception do
      raise EZipFileError.Create('Can''t open: "'+ TheFile + '". Cause: ' + E.Message );
  end;
  if num <> 1 then
      raise EZipFileError.Create('Can''t open: ' + TheFile);
end;

{ TZipConnector }

//-- BG ---------------------------------------------------------- 22.05.2016 --
function TZipConnector.CreateConnection(const Protocol: String): ThtConnection;
begin
  Result := TZipConnection.Create;
end;

//-- BG ---------------------------------------------------------- 22.05.2016 --
class function TZipConnector.GetDefaultProtocols: string;
begin
  Result := 'zip';
end;

//-- BG ---------------------------------------------------------- 22.05.2016 --
class function TZipConnector.GetVersion: string;
begin
  Result := 'VCLZip ' + VclZip.Version;
end;

{$endif}

end.
