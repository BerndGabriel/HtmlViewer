unit FrameViewerReg;

interface

uses classes, HTMLView, FramView, FramBrwz;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('Samples', [THTMLViewer, TFrameViewer, TFrameBrowser]);
end;

end.
