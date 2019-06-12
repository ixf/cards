/**
 * Created by kuba on 2019-05-29
 */

package pl.edu.agh.io.umniedziala.viewController;


import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.embed.swing.SwingFXUtils;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.image.Image;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import pl.edu.agh.io.umniedziala.model.ApplicationEntity;

import javafx.scene.image.ImageView;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import java.util.Map;

public class ChooseColorsViewController {
    private AppController appController;
    private Stage stage;
    private ObservableList<App> observableApps;

    private App changedApp;


    @FXML
    private TableView appColorsTable;

    @FXML
    private TableColumn<App, Image> iconColumn;

    @FXML
    private TableColumn<App, String> applicationColumn;

    @FXML
    private ColorPicker colorPicker;


    @FXML
    public void initialize() {
        appColorsTable.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        applicationColumn.setCellValueFactory(dataValue -> dataValue.getValue().nameProperty());

        iconColumn.setCellValueFactory(data -> new SimpleObjectProperty<Image>(data.getValue().getImage()));
        iconColumn.setCellFactory(param -> {
            final ImageView imageview = new ImageView();
            imageview.setFitHeight(32);
            imageview.setFitWidth(32);

            TableCell<App, Image> cell = new TableCell<App, Image>() {
                @Override
                protected void updateItem(Image item, boolean empty) {
                    if (item != null) {
                        imageview.setImage(item);
                    }
                }
            };
            cell.setGraphic(imageview);
            return cell;
        });

        appColorsTable.getSelectionModel().selectedItemProperty().addListener(new ChangeListener<App>() {
            @Override
            public void changed(ObservableValue<? extends App> observable, App oldValue, App newValue) {
                colorPicker.setValue(Color.valueOf(newValue.getColor()));
                changedApp = newValue;
            }
        });

    }


    public void setAppController(AppController appController) {
        this.appController = appController;
    }


    public void setStage(Stage stage) {
        this.stage = stage;
    }


    public void loadData() {
        List<ApplicationEntity> apps = ApplicationEntity.getAllApplications();
        observableApps = FXCollections.observableArrayList();
        apps.forEach(app -> observableApps.add(new App(app.getName(), app.getColor(), app.getApplicationPath())));
        appColorsTable.setItems(observableApps);
    }


    public void handleOK(ActionEvent event) {
        stage.close();
    }

    public void handleColorChange(ActionEvent event) {
        if (changedApp == null)
            return;
        Color color = colorPicker.getValue();
        String newColor = String.format("#%02X%02X%02X",
                (int) (color.getRed() * 255),
                (int) (color.getGreen() * 255),
                (int) (color.getBlue() * 255));
        ApplicationEntity.updateApplicationColor(changedApp.getName(), newColor);
        changedApp.color = newColor;
    }

    private class App {
        StringProperty name;
        String color;
        private Image image;

        public App(String name, String color, String path) {
            this.name = new SimpleStringProperty(name);
            this.color = color;
            setImageByPath(path);
        }

        private void setImageByPath(String path) {
            try {
                String fixedPath = path.replace('\\', '/');
                File file = new File(fixedPath);

                sun.awt.shell.ShellFolder sf = sun.awt.shell.ShellFolder.getShellFolder(file);
                ImageIcon swingImageIcon = new ImageIcon(sf.getIcon(true));
                BufferedImage bi = new BufferedImage(
                        swingImageIcon.getIconWidth(),
                        swingImageIcon.getIconHeight(),
                        BufferedImage.TYPE_INT_ARGB);
                Graphics g = bi.createGraphics();
                swingImageIcon.paintIcon(null, g, 0, 0);
                g.dispose();
                this.image = SwingFXUtils.toFXImage(bi, null);
            } catch (FileNotFoundException e) {
                e.printStackTrace();
                this.image = new Image("file:krimit.png");
            }
        }

        public String getName() {
            return name.get();
        }

        public StringProperty nameProperty() {
            return name;
        }

        public String getColor() {
            return color;
        }

        public Image getImage() {
            return image;
        }

        public void setImage(Image image) {
            this.image = image;
        }
    }

}