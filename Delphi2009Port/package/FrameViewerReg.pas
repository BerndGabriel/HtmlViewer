unit FrameViewerReg;

interface

uses classes, HTMLView, FramView, FramBrwz;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('PBear HTMLViewer', [THTMLViewer, TFrameViewer, TFrameBrowser]);
end;

end.
