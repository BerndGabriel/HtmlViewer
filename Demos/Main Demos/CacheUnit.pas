{
Version   11.7
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2016 by HtmlViewer Team

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
}
{$ifdef ver140}
{$Warn Symbol_Platform Off}
{$endif}
{$ifdef ver150}  {Delphi 7}
{$Warn Symbol_Platform Off}
{$endif}

unit CacheUnit;

{$include htmlcons.inc}

interface

uses
  {$ifdef LCL}
    LCLIntf, LCLType, LMessages,
  {$else}
    WinTypes, WinProcs,
  {$endif}
  Messages, SysUtils, Classes, Controls, StdCtrls, Math, Contnrs,
  URLSubs,
  HtmlGlobals;

const
  MaxCacheEntries = 200;
  CacheData = 'cachinfo.txt';
var
  Seq: Integer = 10000;

type
  ThtDiskCacheItem = class(TObject)
    FUrl: ThtString;
    FNewUrl: ThtString;
    FFileName: ThtString;
    FDocType: ThtDocType;
    constructor Create(const Url, NewUrl, FileName: ThtString; DocType: ThtDocType);
  end;

type
  ThtDiskCacheFindResult = (dcfrNone, dcfrUrl, dcfrNewUrl);

  ThtDiskCache = class(TObject)
  private
    FItems: TObjectList;
    FDirectory: string;
    procedure CheckSize;
    function MakeName(S: string): string;
    function SplitProtocol(var URL: string): string;
    procedure Erase(I: Integer);
    function IsCacheable(const Protocol: string): Boolean;
    function Find(const S: string; var Index: Integer; var Item: ThtDiskCacheItem ): ThtDiskCacheFindResult;
  public
    constructor Create(const ADirectory: string);
    destructor Destroy; override;
    function AddNameToCache(const URL, NewURL: string; DocType: ThtDocType; Error: Boolean): string;
      {Given an URL, creates a filename and adds both to cachelist}
    function GetCacheFilename(const URL: string; out Filename: string; out DocType: ThtDocType; out NewURL: string): Boolean;
    procedure RemoveFromCache(const URL: string);
    procedure EraseCache;
    procedure Flush;
  end;

implementation

{$ifndef Delphi7_Plus}
uses
  FileCtrl;
{$endif}

//-- BG ---------------------------------------------------------- 21.12.2016 --
constructor ThtDiskCacheItem.Create(const Url, NewUrl, FileName: ThtString; DocType: ThtDocType);
begin
  inherited Create;
  FUrl := Url;
  FNewUrl := NewUrl;
  FFileName := FileName;
  FDocType := DocType;
end;

constructor ThtDiskCache.Create(const ADirectory: string);
var
  F: TextFile;
  I, LineNo, FileSeq: Integer;
  Sep: string;
  S, Url, NewUrl, DocTypeAsString, FileName: string;
  DocType: ThtDocType;
begin
  inherited Create;
  FItems := TObjectList.Create;
  FDirectory := ADirectory;
  ForceDirectories(FDirectory);
  if FileExists(FDirectory + CacheData) then
  begin
    AssignFile(F, FDirectory + CacheData);
    Reset(F);
    Sep := #9;
    LineNo := 0;
    while not EOF(F) do
    begin
      ReadLn(F, S);
      Url := Trim(S);
      I := Pos(Sep, Url);

//      // backward compatibility. We changed the separator from SPACE to TAB
//      if (I = 0) and (LineNo = 0) and (Pos(' ', Url) > 0) then
//      begin
//        Sep := ' ';
//        I := Pos(Sep, Url);
//      end;

      if I > 0 then
      begin
        NewUrl := Trim(Copy(Url, I + 1, MaxInt));
        SetLength(Url, I - 1);
        I := Pos(Sep, NewUrl);
        if I > 0 then
        begin
          DocTypeAsString := Trim(Copy(NewUrl, I, MaxInt));
          SetLength(NewUrl, I - 1);
          I := Pos(Sep, DocTypeAsString);
          if I > 0 then
          begin
            FileName := Trim(Copy(DocTypeAsString, I, MaxInt));
            SetLength(DocTypeAsString, I - 1);
            DocType := ThtDocType(StrToIntDef(DocTypeAsString, Ord(HTMLType)));
            FileSeq := StrToIntDef(Copy(FileName, 2, 5), 10000);
            if Seq <= FileSeq then
              Seq := FileSeq + 1;
            FItems.Add(ThtDiskCacheItem.Create(Url, NewUrl, FileName, DocType));
          end;
        end;
      end;
      Inc(LineNo);
    end;
    CloseFile(F);
  end;
end;

destructor ThtDiskCache.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

//-- BG ---------------------------------------------------------- 21.12.2016 --
function ThtDiskCache.Find(const S: string; var Index: Integer; var Item: ThtDiskCacheItem): ThtDiskCacheFindResult;
var
  I: Integer;
begin
  Result := dcfrNone;
  for I := 0 to FItems.Count - 1 do
    with ThtDiskCacheItem(FItems[I]) do
      if FUrl = S then
      begin
        Index := I;
        Item := ThtDiskCacheItem(FItems[I]);
        Result := dcfrUrl;
        Exit;
      end
      else if FNewUrl = S then
      begin
        Index := I;
        Item := ThtDiskCacheItem(FItems[I]);
        Result := dcfrNewUrl;
        // continue search and prefer item with FUrl = S
      end;

end;

//-- BG ---------------------------------------------------------- 20.05.2016 --
// extracted from Destroy
procedure ThtDiskCache.Flush;
var
  F: TextFile;
  I: Integer;
begin
  AssignFile(F, FDirectory + CacheData);
  Rewrite(F);
  for I := 0 to FItems.Count-1 do
    with ThtDiskCacheItem(FItems[I]) do
      WriteLn(F, FUrl, #9, FNewUrl, #9, IntToStr(Ord(FDocType)), #9, FFileName);
  CloseFile(F);
end;

function ThtDiskCache.IsCacheable(const Protocol: string): Boolean;
begin
  Result := Pos('http', Protocol) = 1;  {cache only http's and https's}
end;

function ThtDiskCache.AddNameToCache(const URL, NewURL: string; DocType: ThtDocType; Error: Boolean): string;
var
  S, P: string;
  I: Integer;
  Item: ThtDiskCacheItem;
begin
  S := URL;
  P := SplitProtocol(S);
  SetLength(Result, 0);
  if IsCacheable(P) then
  begin
    case Find(S, I, Item) of
      dcfrUrl,
      dcfrNewUrl:
        Result := Item.FFileName;
    else
      Result := MakeName(S);
      if Error then
        S := S + '*';   {so it won't be found in a lookup}

      if Length(NewURL) > 0 then
        P := NewURL
      else
        P := '*';
      FItems.Add(ThtDiskCacheItem.Create(S, P, Result, DocType));
      CheckSize;
    end;
    Result := FDirectory + Result;
  end;
end;

function ThtDiskCache.GetCacheFilename(const URL: string; out Filename: string; out DocType: ThtDocType; out NewURL: string): Boolean;
var
  S, P: string;
  I: Integer;
  Item: ThtDiskCacheItem;
begin
  S := URL;
  P := SplitProtocol(S);
  Item := nil;
  SetLength(NewURL, 0);
  if IsCacheable(P) then
    case Find(S, I, Item) of
      dcfrUrl:
        if Item.FNewUrl <> '*' then
          NewURL := Item.FNewUrl;
    end;

  Result := Item <> nil;
  if Result then
  begin
    Filename := FDirectory + Item.FFileName;
    DocType := Item.FDocType;
  end
  else
  begin
    SetLength(Filename, 0);
    DocType := HTMLType;
  end;
end;

procedure ThtDiskCache.CheckSize;
begin
  while FItems.Count > MaxCacheEntries do
    Erase(0);
end;

function ThtDiskCache.MakeName(S: string): string;
{S is lower case here}
var
  I: Integer;
  Ext: string;
  GoodExt: Boolean;
begin
  Ext := '';
  for I := Length(S) downto Max(1, Length(S)-5) do
    if S[I] = '.' then
    begin
      Ext := Copy(S, I, MaxInt);
      break;
    end;

  for I := 2 to Length(Ext) do
    if not IsAlpha(Ext[I]) then
    begin
      SetLength(Ext, I - 1);
      break;
    end;

  GoodExt := Length(Ext) > 1;
//           ((Ext = '.htm') or (Ext = '.html') or (Ext = '.xhtml') or (Ext = '.xht') or
//            (Ext = '.gif') or (Ext = '.jpg') or
//            (Ext = '.png') or (Ext = '.bmp') or (Ext = '.txt') or (Ext = '.jpeg') or
//            (Ext = '.css'));
  Result := 'm' + IntToStr(Seq);
  Inc(Seq);
  if GoodExt then
    Result := Result + Ext;
end;

procedure ThtDiskCache.Erase(I: Integer);
begin
  DeleteFile(FDirectory + ThtDiskCacheItem(FItems[I]).FFileName);
  FItems.Delete(I);
end;

procedure ThtDiskCache.RemoveFromCache(const URL: string);
var
  I: Integer;
  S: string;
  Item: ThtDiskCacheItem;
begin
  S := URL;
  SplitProtocol(S);
  case Find(S, I, Item) of
    dcfrUrl:
      Erase(I);
  end;
end;

procedure ThtDiskCache.EraseCache;
var
  I: Integer;
begin
  for I := FItems.Count-1 downto 0 do
    Erase(I);
end;

function ThtDiskCache.SplitProtocol(var URL: string): string;
var
  I: Integer;
begin
  I := 1;
  if FindSchemeSep(Url, I) then
  begin
    Result := LowerCase(Copy(Url, 1, I - 1));
    Delete(URL, 1, I + 2);
  end
  else
    // default protocol
    Result := 'http';

  if CompareText('www.', Copy(URL, 1, 4)) = 0 then
    Delete(URL, 1, 4);
end;

end.
