unit startUI;

interface

uses
  UIContainer, Controls, Classes;

type
  TClientStart = procedure (Host : string; Port : integer) of object;

  TStartUI = class (TUIContainer)
    constructor Create (AOwner: TComponent); override;
  end;

implementation

{ TStartUI }

constructor TStartUI.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Start UI';
end;

end.
