package pl.edu.agh.io.umniedziala.viewController;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.stage.Stage;
import pl.edu.agh.io.umniedziala.model.CustomEventEntity;
import pl.edu.agh.io.umniedziala.view.DateTimePicker;

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
    public void handleSaveButton(ActionEvent event){
        if (nameInserted && startTimeInserted && endTimeInserted){
            CustomEventEntity.create(startTime.getDateTimeValue().toString(),endTime.getDateTimeValue().toString(),
                    nameInput.getText(), descriptionInput.getText(),colorPicker.getCustomColors().toString());
            stage.close();
        } else{
            alert.setText("Choose start time, end time and name!");
        }
    }
}
