/**
 * Created by kuba on 06/04/2019
 */

package pl.edu.agh.io.umniedziala.viewController;


import javafx.fxml.FXML;

public class MainViewController {
    private AppController appController;

    @FXML
    public void initialize(){

    }

    public void setAppController(AppController appController) {
        this.appController = appController;
    }

}