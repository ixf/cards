package pl.edu.agh.io.umniedziala.viewController;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import pl.edu.agh.io.umniedziala.model.CustomEventEntity;
import pl.edu.agh.io.umniedziala.view.DateTimePicker;
import sun.java2d.pipe.SpanShapeRenderer;

import java.text.SimpleDateFormat;
import java.time.format.DateTimeFormatter;

public class CustomEventController {

    private AppController appController;
    private Stage stage;

    Boolean nameInserted;
    Boolean startTimeInserted;
    Boolean endTimeInserted;

    @FXML
    private Label alert;

    @FXML
    private TextField nameInput;

    @FXML
    private TextArea descriptionInput;

    @FXML
    private ColorPicker colorPicker;

    @FXML
    private Button saveButton;

    @FXML
    private DateTimePicker startTime;

    @FXML
    private DateTimePicker endTime;

    public void setAppController(AppController appController) {
        this.appController = appController;
    }

    public void setStage(Stage stage) {
        this.stage = stage;
    }

    @FXML
    public void initialize(){
        nameInserted=false;
        startTimeInserted=false;
        endTimeInserted=false;
    }

    @FXML
    public void nameInputHandler(ActionEvent event){
        nameInserted=true;
    }

    @FXML
    public void startTimeHandler(ActionEvent event){
        startTimeInserted=true;
    }

    @FXML
    public void endTimeHandler(ActionEvent event){
        endTimeInserted=true;
    }

    @FXML
    public void handleSaveButton(ActionEvent event) {
        if (nameInserted && startTimeInserted && endTimeInserted) {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
            String start = startTime.getDateTimeValue().format(formatter);
            String end = endTime.getDateTimeValue().format(formatter);
            Color color = colorPicker.getValue();
            String resultColor = String.format("#%02X%02X%02X",
                    ((int)color.getRed())*255,
                    ((int)color.getGreen())*255,
                    ((int)color.getBlue())*255);
            CustomEventEntity.create(start, end, nameInput.getText(), descriptionInput.getText(), resultColor);
            stage.close();
        } else {
            alert.setText("Choose start time, end time and name!");
        }
    }
}
