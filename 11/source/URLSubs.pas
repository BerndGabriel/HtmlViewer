{
Version   10.2
Copyright (c) 1995-2008 by L. David Baldwin, 2008-2010 by HtmlViewer Team

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
unit URLSubs;

interface

{$I htmlcons.inc}

{***************************************************************************************************
 * URL processing methods
 **************************************************************************************************}

procedure ParseURL(const url: string; var Proto, User, Pass, Host, Port, Path: string);
{François PIETTE's URL parsing procedure}

procedure SplitString(var Str: string; Sep: Char; out Spall: string);
{Split Str at first appearance of Sep into Str and Spall. Spall starts with Sep.}

procedure SplitDest(const Src: string; out Name, Dest: string); overload;
procedure SplitDest(var Src: string; out Dest: string); overload;
{Split local destination from an URL at first appearance of '#', returns Dest with separating '#'}

procedure SplitQuery(const Src: string; out Name, Query: string); overload;
procedure SplitQuery(var Src: string; out Query: string); overload;
{Split query from an URL at first appearance of '?', returns Query with separating '?'}

function GetBase(const URL: string): string; deprecated;
function GetURLBase(const URL: string): string;
{Given an URL, get the base directory}

function Combine(Base, APath: string): string; deprecated;
function CombineURL(Base, APath: string): string;
{combine a base and a path taking into account that overlap might exist}
{needs work for cases where directories might overlap}

function IsFullURL(const URL: string): boolean;
{set if contains a protocol}

function GetProtocol(const URL: string): string;
{return the http, mailto, etc in lower case}

function GetURLExtension(const URL: string): string;
{returns extension without the '.', mixed case}

function GetURLFilenameAndExt(const URL: string): string;
{returns mixed case after last /}


{***************************************************************************************************
 * both URL and DOS processing methods
 **************************************************************************************************}

function Normalize(const URL: string): string;
{lowercase, trim, and make sure a '/' terminates a hostname, adds http://}

function DosToHTML(FName: string): string;
{convert an Dos style filename to one for HTML.  Does not add the file:///}

function DosToHtmlNoSharp(FName: string): string;
{convert a Dos style filename to one for HTML.  Does not add the file:///.
 use where "#" is not being used as a local position but rather is part of filename.}

function HTMLToDos(FName: string): string;
{convert an HTML style filename to one for Dos}


{***************************************************************************************************
 * DOS processing methods
 **************************************************************************************************}

function CombineDos(const Base, Path: string): string;
{combine a base and a path}


{**************************************************************************************************}
implementation

uses
  SysUtils, Math;

{----------------GetURLBase}

function GetURLBase(const URL: string): string;
{Given an URL, get the base directory}
var
  I, J, Q: integer;
begin
  Result := Trim(URL);
  Q := Pos('?', Result);
  if Q = 0 then
    Q := Length(Result);
  J := Pos('//', Result);
  if (J = 0) or (J > Q) then
    J := 1;
  for I := Q downto J do
    if Result[I] = '/' then
    begin
      SetLength(Result, I);
      Exit;
    end;
  Result := Result + '/';
end;

function GetBase(const URL: string): string;
begin
  Result := GetURLBase(URL);
end;

{----------------Combine}
function CombineDos(const Base, Path: string): string;
var
  L: Integer;
begin
  L := Length(Base);
  if (L > 0) and (Base[L] = '\') then
    if (Length(Path) > 0) and (Path[1] = '\') then
      Result := Copy(Base, 1, L - 1) + Path
    else
      Result := Base + Path
  else
    if (Length(Path) > 0) and (Path[1] = '\') then
      Result := Base + Path
    else
      Result := Base + '\' + Path;
end;

function Combine(Base, APath: string): string;
begin
  Result := CombineURL(Base, APath);
end;

function CombineURL(Base, APath: string): string;
{combine a base and a path taking into account that overlap might exist}
{needs work for cases where directories might overlap}
var
  I, J, K: integer;
  Proto: string;
begin
  Base := Trim(Base);
  Proto := GetProtocol(Base);
  J := Pos('file:///', Base);
  if J = 1 then
    J := Pos('/', Copy(Base, 9, Length(Base) - 8)) + 8 {fourth slash for file:///c|/ }
  else
  begin
    J := Pos('://', Base);
    if J > 0 then
      J := Pos('/', Copy(Base, J + 3, Length(Base) - (J + 2))) + J + 2 {third slash for http://www.xxx.com/ }
    else
      J := Pos('/', Base);
  end;
{insure there is a '/' on the end}
  if J = 0 then
  begin
    Base := Base + '/'; {needs a slash}
    J := Length(Base);
  end
  else if Base[Length(Base)] <> '/' then
    Base := Base + '/';

  APath := Trim(APath);

  if (APath <> '') and (APath[1] = '/') then
  begin {remove path from base and use host only}
    if Pos('//', APath) = 1 then {UNC filename}
      if Proto = 'file' then
        Result := 'file:///' + APath
      else
        Result := Proto + ':' + APath
    else
      Result := Copy(Base, 1, J) + Copy(APath, 2, Length(APath) - 1);
  end
  else
    Result := Base + APath;

{remove any '..\'s to simply and standardize for cacheing}
  I := Pos('/../', Result);
  while I > 0 do
  begin
    if I > J then
    begin
      K := I;
      while (I > 1) and (Result[I - 1] <> '/') do
        Dec(I);
      if I <= 1 then
        Break;
      Delete(Result, I, K - I + 4); {remove canceled directory and '/../'}
    end
    else
      Delete(Result, I + 1, 3); {remove '../' after host name}
    I := Pos('/../', Result);
  end;
{remove any './'s}
  I := Pos('/./', Result);
  while I > 0 do
  begin
    Delete(Result, I + 1, 2);
    I := Pos('/./', Result);
  end;
end;

function Normalize(const URL: string): string;
{trim, and make sure a '/' terminates a hostname and http:// is present.
 In other words, if there is only 2 /'s, put one on the end}
var
  I, J, LastSlash: integer;
  Query: string;
begin
  Result := Trim(URL);
  J := Pos('?', Result);
  if J > 0 then
  begin
    Query := Copy(Result, J, Length(Result) - J + 1);
    Delete(Result, J, Length(Result) - J + 1);
  end
  else
    Query := '';
  if Pos('file://', Result) = 1 then
    Result := DosToHTML(Result);
  if GetProtocol(Result) = '' then
    Result := 'http://' + Result; {add http protocol as a default}
  J := Pos('/./', Result);
  while J > 0 do
  begin
    Delete(Result, J + 1, 2); {remove './'s}
    J := Pos('/./', Result);
  end;
  J := Pos('//', Result);
  LastSlash := 0;
  for I := J + 2 to Length(Result) do
    if Result[I] = '/' then
      LastSlash := I;
  if LastSlash = 0 then
    Result := Result + '/';
  Result := Result + Query;
end;

function IsFullURL(const URL: string): boolean;
var
  N: integer;
  S: string;
begin
  N := Pos('?', Url);
  if N > 0 then
    S := Copy(Url, 1, N - 1)
  else
    S := Url;
  N := Pos('://', S);
  Result := (N > 0) and (N < Pos('/', S)) or (Pos('mailto:', Lowercase(S)) <> 0);
end;

function GetProtocol(const URL: string): string;
var
  User, Pass, Port, Host, Path: string;
  S: string;
  I: integer;
begin
  I := Pos('?', URL);
  if I > 0 then
    S := Copy(URL, 1, I - 1)
  else
    S := URL;
  ParseURL(S, Result, user, pass, Host, port, Path);
  Result := Lowercase(Result);
end;

function GetURLExtension(const URL: string): string;
var
  I, N: integer;
begin
  Result := '';
  I := Pos('?', URL);
  if I > 0 then
    N := I - 1
  else
    N := Length(URL);
  I := Pos('#', URL);
  if (I > 0) and (I < N) then
    N := I - 1;
  for I := N downto Max(1, N - 5) do
    if URL[I] = '.' then
    begin
      Result := Copy(URL, I + 1, N - I);
      Break;
    end;
end;

function GetURLFilenameAndExt(const URL: string): string;
var
  I: integer;
begin
  Result := URL;
  for I := Length(URL) downto 1 do
    if URL[I] = '/' then
    begin
      Result := Copy(URL, I + 1, 255);
      Break;
    end;
end;

{ Find the count'th occurence of the s string in the t string.              }
{ If count < 0 then look from the back                                      }
{Thanx to François PIETTE}

function Posn(const s, t: string; Count: Integer): Integer;
var
  i, h, Last: Integer;
  u: string;
begin
  u := t;
  if Count > 0 then
  begin
    Result := Length(t);
    for i := 1 to Count do
    begin
      h := Pos(s, u);
      if h > 0 then
        u := Copy(u, h + 1, Length(u))
      else
      begin
        u := '';
        Inc(Result);
      end;
    end;
    Result := Result - Length(u);
  end
  else if Count < 0 then
  begin
    Last := 0;
    for i := Length(t) downto 1 do
    begin
      u := Copy(t, i, Length(t));
      h := Pos(s, u);
      if (h <> 0) and ((h + i) <> Last) then
      begin
        Last := h + i - 1;
        Inc(count);
        if Count = 0 then
          break;
      end;
    end;
    if Count = 0 then
      Result := Last
    else
      Result := 0;
  end
  else
    Result := 0;
end;

{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path         }
{Thanx to François PIETTE}

procedure ParseURL(
  const url: string;
  var Proto, User, Pass, Host, Port, Path: string);
var
  p, q: Integer;
  s: string;
begin
  proto := '';
  User := '';
  Pass := '';
  Host := '';
  Port := '';
  Path := '';

  if Length(url) < 1 then
    Exit;

  p := pos('://', url);
  if p = 0 then
  begin
    if (url[1] = '/') then
    begin
            { Absolute path without protocol specified }
      proto := 'http';
      p := 1;
      if (Length(url) > 1) and (url[2] <> '/') then
      begin
                { Relative path }
        Path := Copy(url, 1, Length(url));
        Exit;
      end;
    end
    else if lowercase(Copy(url, 1, 5)) = 'http:' then
    begin
      proto := 'http';
      p := 6;
      if (Length(url) > 6) and (url[7] <> '/') then
      begin
                { Relative path }
        Path := Copy(url, 6, Length(url));
        Exit;
      end;
    end
    else if lowercase(Copy(url, 1, 7)) = 'mailto:' then
    begin
      proto := 'mailto';
      p := pos(':', url);
    end;
  end
  else
  begin
    proto := Copy(url, 1, p - 1);
    inc(p, 2);
  end;
  s := Copy(url, p + 1, Length(url));

  p := pos('/', s);
  if p = 0 then
    p := Length(s) + 1;
  Path := Copy(s, p, Length(s));
  s := Copy(s, 1, p - 1);

  p := Posn(':', s, -1);
  if p > Length(s) then
    p := 0;
  q := Posn('@', s, -1);
  if q > Length(s) then
    q := 0;
  if (p = 0) and (q = 0) then
  begin { no user, password or port }
    Host := s;
    Exit;
  end
  else if q < p then
  begin { a port given }
    Port := Copy(s, p + 1, Length(s));
    Host := Copy(s, q + 1, p - q - 1);
    if q = 0 then
      Exit; { no user, password }
    s := Copy(s, 1, q - 1);
  end
  else
  begin
    Host := Copy(s, q + 1, Length(s));
    s := Copy(s, 1, q - 1);
  end;
  p := pos(':', s);
  if p = 0 then
    User := s
  else
  begin
    User := Copy(s, 1, p - 1);
    Pass := Copy(s, p + 1, Length(s));
  end;
end;

//-- BG ---------------------------------------------------------- 28.11.2010 --
procedure SplitString(var Str: string; Sep: Char; out Spall: string);
// extracted from several locations spread all over the code.
var
  I: Integer;
begin
  I := Pos(Sep, Str) - 1; // '-1' will be needed in both copy()s and comparing with 0 is faster.
  if I >= 0 then
  begin
    Spall := System.Copy(Str, I + 1, Length(Str) - I);
    Str   := System.Copy(Str, 1, I);
  end
  else
    Spall := '';
end;

procedure SplitDest(var Src: string; out Dest: string); overload;
begin
  SplitString(Src, '#', Dest);
end;

procedure SplitDest(const Src: string; out Name, Dest: string); overload;
{Split an URL into filename and Destination}
begin
  Name := Src;
  SplitString(Name, '#', Dest);
end;

procedure SplitQuery(var Src: string; out Query: string); overload;
begin
  SplitString(Src, '?', Query);
end;

procedure SplitQuery(const Src: string; out Name, Query: string); overload;
begin
  Name := Src;
  SplitString(Name, '?', Query);
end;

function DosToHTML(FName: string): string;
{convert a Dos style filename to one for HTML.  Does not add the file:///}
var
  Colon: integer;

  procedure Replace(Old, New: char);
  var
    I: integer;
  begin
    I := Pos(Old, FName);
    while I > 0 do
    begin
      FName[I] := New;
      I := Pos(Old, FName);
    end;
  end;

begin
  Colon := Pos('://', FName);
  Replace(':', '|');
  Replace('\', '/');
  if Colon > 0 then
    FName[Colon] := ':'; {return it to a colon}
  Result := FName;
end;

function DosToHtmlNoSharp(FName: string): string;
{convert a Dos style filename to one for HTML.  Does not add the file:///.
 use where "#" is not being used as a local position but rather is part of filename.}
var
  I: integer;
begin
  I := Pos('#', FName);
  while I > 0 do
  begin
    Delete(FName, I, 1);
    Insert('%23', FName, I);
    I := Pos('#', FName);
  end;
  Result := FName;
end;

function HTMLToDos(FName: string): string;
{convert an HTML style filename to one for Dos}
var
  I: integer;

  procedure Replace(Old, New: char);
  var
    I: integer;
  begin
    I := Pos(Old, FName);
    while I > 0 do
    begin
      FName[I] := New;
      I := Pos(Old, FName);
    end;
  end;

  procedure ReplaceEscapeChars;
  var
    S: string;
    I: integer;
  begin
    I := Pos('%', FName);
    while (I > 1) and (I <= Length(FName) - 2) do
    begin
      S := '$' + FName[I + 1] + FName[I + 2];
      try
        FName[I] := chr(StrToInt(S));
        Delete(FName, I + 1, 2);
      except {ignore exception}
        Exit;
      end;
      I := Pos('%', FName);
    end;
  end;

begin
  ReplaceEscapeChars;
  I := pos('/', FName);
  if I <> 0 then
  begin
    I := Pos('file:///', Lowercase(FName));
    if I > 0 then
      System.Delete(FName, I, 8)
    else
    begin
      I := Pos('file://', Lowercase(FName));
      if I > 0 then
        System.Delete(FName, I, 7)
      else
      begin
        I := Pos('file:/', Lowercase(FName));
        if I > 0 then
          System.Delete(FName, I, 6);
      end;
    end;
    Replace('|', ':');
    Replace('/', '\');
  end;
  Result := FName;
end;

end.
