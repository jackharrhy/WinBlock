unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    SelectedImage: TPicture;
    SelectedImageIndex: Integer;
    GridState: array[0..15, 0..15] of Integer;
    GridImages: array[0..15, 0..15] of TImage;
    PaletteImages: TList;
    PaletteFilenames: TStringList;
    SelectedShape: TShape;
    ExportMemo: TMemo;
    procedure LoadToPanel(const Dir: string);
    procedure CreateGrid;
    procedure CreateExportUI;
    procedure PaletteImageClick(Sender: TObject);
    procedure GridImageClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpdateExportText;
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
        Img.OnClick := @PaletteImageClick;
        PaletteImages.Add(Img);
        PaletteFilenames.Add(ChangeFileExt(SR.Name, ''));
        
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

procedure TMainForm.CreateGrid;
var
  Col, Row: Integer;
  CellSize: Integer;
  Img: TImage;
begin
  CellSize := Panel3.Width div 16;
  
  for Row := 0 to 15 do
    for Col := 0 to 15 do
    begin
      Img := TImage.Create(Panel3);
      Img.Parent := Panel3;
      Img.Left := Col * CellSize;
      Img.Top := Row * CellSize;
      Img.Width := CellSize;
      Img.Height := CellSize;
      Img.Stretch := True;
      Img.Picture.Bitmap := nil;
      GridImages[Col, Row] := Img;
      GridState[Col, Row] := -1;
      
      Img.OnMouseDown := @GridImageClick;
    end;
end;

procedure TMainForm.CreateExportUI;
begin
  ExportMemo := TMemo.Create(Panel2);
  ExportMemo.Parent := Panel2;
  ExportMemo.Left := 10;
  ExportMemo.Top := 10;
  ExportMemo.Width := Panel2.Width - 20;
  ExportMemo.Height := Panel2.Height - 20;
  ExportMemo.ScrollBars := ssVertical;
  ExportMemo.WordWrap := False;
  ExportMemo.ReadOnly := True;
  ExportMemo.Text := 'Discord export will appear here...';
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PaletteImages := TList.Create;
  PaletteFilenames := TStringList.Create;
  SelectedImage := nil;
  SelectedImageIndex := -1;
  SelectedShape := nil;
  LoadToPanel('.\bin');
  CreateGrid;
  CreateExportUI;
end;

procedure TMainForm.PaletteImageClick(Sender: TObject);
var
  Img: TImage;
  I: Integer;
begin
  if Sender is TImage then
  begin
    if Assigned(SelectedImage) then
      SelectedImage.Free;
      
    SelectedImage := TPicture.Create;
    SelectedImage.Assign(TImage(Sender).Picture);
    
    for I := 0 to PaletteImages.Count - 1 do
    begin
      if TImage(PaletteImages[I]) = Sender then
      begin
        SelectedImageIndex := I;
        Break;
      end;
    end;
    
    if Assigned(SelectedShape) then
      SelectedShape.Free;
    
    SelectedShape := TShape.Create(Panel1);
    SelectedShape.Parent := Panel1;
    SelectedShape.Shape := stRectangle;
    SelectedShape.Brush.Style := bsClear;
    SelectedShape.Pen.Color := clBlue;
    SelectedShape.Pen.Width := 2;
    SelectedShape.Left := TImage(Sender).Left - 2;
    SelectedShape.Top := TImage(Sender).Top - 2;
    SelectedShape.Width := TImage(Sender).Width + 4;
    SelectedShape.Height := TImage(Sender).Height + 4;
    SelectedShape.BringToFront;
    TImage(Sender).BringToFront;
  end;
end;

procedure TMainForm.GridImageClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
  CellSize: Integer;
  Img: TImage;
begin
  if Sender is TImage then
  begin
    Img := TImage(Sender);
    CellSize := Panel3.Width div 16;
    Col := Img.Left div CellSize;
    Row := Img.Top div CellSize;
    
    if (Col >= 0) and (Col <= 15) and (Row >= 0) and (Row <= 15) then
    begin
      if Button = mbLeft then
      begin
        if Assigned(SelectedImage) then
        begin
          Img.Picture.Assign(SelectedImage);
          GridState[Col, Row] := SelectedImageIndex;
          UpdateExportText;
        end;
      end
      else if Button = mbRight then
      begin
        Img.Picture.Bitmap := nil;
        GridState[Col, Row] := -1;
        UpdateExportText;
      end;
    end;
  end;
end;

procedure TMainForm.UpdateExportText;
var
  Row, Col: Integer;
  ExportText: string;
  CellValue: Integer;
begin
  ExportText := '';
  
  for Row := 0 to 15 do
  begin
    for Col := 0 to 15 do
    begin
      CellValue := GridState[Col, Row];
      if CellValue >= 0 then
      begin
        if (CellValue < PaletteFilenames.Count) then
          ExportText := ExportText + ':' + PaletteFilenames[CellValue] + ':'
        else
          ExportText := ExportText + ':unknown:';
      end;
    end;
    if Row < 15 then
      ExportText := ExportText + #13#10;
  end;
  
  if ExportText = '' then
    ExportText := 'Discord export will appear here...'
  else
    ExportMemo.Text := ExportText;
end;

end.

