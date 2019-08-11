unit UIContainer;

interface

uses
  ExtCtrls, Controls, Classes;

type
  TClientPrintMethod = procedure (t, s : string) of object;

  TUIContainer = class (TPanel)
    constructor Create (AOwner : TComponent); override;
  end;

implementation

{ TUIContainer }

constructor TUIContainer.Create(AOwner: TComponent);
begin
  inherited;
  Align := alClient;
  BevelEdges := [];
  BevelOuter := bvNone;
  ParentBackground := false;
end;

end.
