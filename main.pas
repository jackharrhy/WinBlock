unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadToPanel(const Dir: string);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.LoadToPanel(const Dir: string);
var
  SR: TSearchRec;
  Pic: TPicture;
  Img: TImage;
  Col, Row: Integer;
  ImagesPerRow: Integer;
begin
  ImagesPerRow := Panel1.Width div 32;
  Col := 0;
  Row := 0;
  
  if FindFirst(Dir + PathDelim + '*.*', faAnyFile, SR) = 0 then
  repeat
    if (SR.Attr and faDirectory) = 0 then
    begin
      Pic := TPicture.Create;
      try
        Pic.LoadFromFile(Dir + PathDelim + SR.Name);

        Img := TImage.Create(Panel1);
        Img.Picture.Assign(Pic);
        Img.Parent := Panel1;
        Img.Left := Col * 32;
        Img.Top := Row * 32;
        Img.Stretch := True;
        Img.Width := 32;
        Img.Height := 32;
        
        Inc(Col);
        if Col >= ImagesPerRow then
        begin
          Col := 0;
          Inc(Row);
        end;
        
      except
        Pic.Free;
      end;
    end;
  until FindNext(SR) <> 0;
  FindClose(SR);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  LoadToPanel('.\bin');
end;

end.

