package pl.edu.agh.io.umniedziala.view;

import javafx.beans.NamedArg;
import javafx.collections.FXCollections;
import javafx.event.EventHandler;
import javafx.scene.Node;
import javafx.scene.chart.*;
import javafx.scene.input.ScrollEvent;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.util.StringConverter;
import pl.edu.agh.io.umniedziala.model.RunningPeriodEntity;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

public class TimeChart extends XYChart<Number, String> {

    // granice w których wyświetlamy wyrkes
    // może to się będzie jakoś ustawiać a może zoomować/przesuwać wykres i będzie zbędne
    private static final int minHour = 8;
    private static final int maxHour = 24;

    // powinno się dynamicznie zmieniać może? TODO
    private static final double lineHeight = 80.0;

    // YAxis zawiera stringi: TODO zamienić to na inty i formatować na stringi korzystając z appNames
    private Map<Integer, String> appNames;
    private Map<Integer, XYChart.Series> seriesMap = new HashMap<>();

    public static class ExtraData {

        private double length;
        private Color style;

        public ExtraData(double length, Color style) {
            super();
            this.length = length;
            this.style = style;
        }

        public Color getStyle() {
            return style;
        }

        public void setStyle(Color style) {
            this.style = style;
        }

        public double getLength() {
            return length;
        }

        public void setLength(double length) {
            this.length = length;
        }
    }

    public TimeChart(@NamedArg("xAxis") NumberAxis timeAxis,
                     @NamedArg("yAxis") CategoryAxis appAxis) {
        super(timeAxis, appAxis);
        setData(FXCollections.observableArrayList());
        timeAxis.setLowerBound(minHour);
        setLegendVisible(false);

        // oś czasu przechowuje godziny dnia ( double 0-24) i wyświetla je po formatowaniu do stringa
        // wczesniej przechowywalismy sekundy ale tak Ticki beda sie przemieszczac z przesuwaniem
        timeAxis.setUpperBound(maxHour);
        timeAxis.setTickUnit(1.0);

        timeAxis.setTickLabelFormatter(new StringConverter<Number>() {
            @Override
            public String toString(Number object) {
                SimpleDateFormat sdf = new SimpleDateFormat("HH:mm");
                sdf.setTimeZone(TimeZone.getTimeZone("UTC")); // koniecznie UTC. tylko do formatowania!
                long value = (long) (object.doubleValue() * 3600.0 * 1000.0);
                // zamiana s na ms
                return sdf.format(new Date(value));
            }

            @Override
            public Number fromString(String string) {
                return 0L;
            }
        });

        final double[] lastMouseX = {0};

        setOnMousePressed(event -> lastMouseX[0] = event.getX());

        setOnMouseDragged(event -> {
            event.consume();
            double pastMinHour = timeAxis.getLowerBound();
            double pastMaxHour = timeAxis.getUpperBound();

            double delta = (lastMouseX[0] - event.getX()) / timeAxis.getWidth() * (maxHour - minHour);
            if (pastMinHour + delta < 0)
                delta = -1 * pastMinHour;
            else if (pastMaxHour + delta > 24) {
                delta = -1 * pastMaxHour + 24;
            }
            timeAxis.setLowerBound(pastMinHour + delta);
            timeAxis.setUpperBound(pastMaxHour + delta);

            lastMouseX[0] = event.getX();
        });
    }

    public void setAppNames(Map<Integer, String> appNames) {
        this.appNames = appNames;
        for (Map.Entry<Integer, String> ent : appNames.entrySet()) {
            XYChart.Series series = new XYChart.Series();
            series.setName(ent.getValue());
            seriesMap.put(ent.getKey(), series);

            getData().add(series);
        }
    }

    public void setDataByResults(List<RunningPeriodEntity> results) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));

        for (Series series : seriesMap.values()) {
            series.getData().clear();
        }

        for (RunningPeriodEntity ent : results) {
            XYChart.Series series = seriesMap.get(ent.getApplicationId());
            Double start = 0.0;
            Double length = 1.0;
            try {
                Double end = (double) sdf.parse(ent.getEndTime()).getTime() % 86400000 / 1000.0;
                start = (double) sdf.parse(ent.getStartTime()).getTime() % 86400000 / 1000.0;
                length = end - start; // czas w sekundach
            } catch (ParseException e) {
                e.printStackTrace();
            }
            start /= 3600.0;
            length /= 3600.0;
            String appName = appNames.get(ent.getApplicationId());
            series.getData().add(new XYChart.Data<Number, String>(start, appName, new ExtraData(length, Color.RED)));
        }
    }

    @Override
    protected void layoutPlotChildren() {

        for (int seriesIndex = 0; seriesIndex < getData().size(); seriesIndex++) {

            Series<Number, String> series = getData().get(seriesIndex);

            Iterator<Data<Number, String>> iter = getDisplayedDataIterator(series);
            while (iter.hasNext()) {
                Data<Number, String> item = iter.next();
                double x = getXAxis().getDisplayPosition(item.getXValue());
                double y = getYAxis().getDisplayPosition(item.getYValue());
                if (Double.isNaN(x) || Double.isNaN(y)) {
                    continue;
                }
                Node node = item.getNode();
                Rectangle box;
                if (node instanceof StackPane) {
                    StackPane region = (StackPane) item.getNode();
                    if (region.getShape() == null) {
                        box = new Rectangle(((ExtraData) item.getExtraValue()).getLength(), lineHeight);
                    } else if (region.getShape() instanceof Rectangle) {
                        box = (Rectangle) region.getShape();
                    } else {
                        return;
                    }
                    box.setWidth(((ExtraData) item.getExtraValue()).getLength() * Math.abs(((NumberAxis) getXAxis()).getScale()));
                    box.setHeight(lineHeight);
                    y -= lineHeight / 2.0;

                    region.setShape(null);
                    region.setShape(box);
                    region.setScaleShape(false);
                    region.setCenterShape(false);
                    region.setCacheShape(false);

                    node.setLayoutX(x);
                    node.setLayoutY(y);
                }
            }
        }
    }

    @Override
    protected void dataItemAdded(Series<Number, String> series, int itemIndex, Data<Number, String> item) {
        Node block = createContainer(series, getData().indexOf(series), item, itemIndex);
        getPlotChildren().add(block);
    }

    @Override
    protected void dataItemRemoved(final Data<Number, String> item, final Series<Number, String> series) {
        final Node block = item.getNode();
        getPlotChildren().remove(block);
        removeDataItemFromDisplay(series, item);
    }

    @Override
    protected void dataItemChanged(Data<Number, String> item) {
        // nie wiem co robić
    }

    @Override
    protected void seriesAdded(Series<Number, String> series, int seriesIndex) {
        for (int j = 0; j < series.getData().size(); j++) {
            Data<Number, String> item = series.getData().get(j);
            Node container = createContainer(series, seriesIndex, item, j);
            getPlotChildren().add(container);
        }
    }

    @Override
    protected void seriesRemoved(final Series<Number, String> series) {
        for (XYChart.Data<Number, String> d : series.getData()) {
            final Node container = d.getNode();
            getPlotChildren().remove(container);
        }
        removeSeriesFromDisplay(series);
    }


    private Node createContainer(Series<Number, String> series, int seriesIndex, final Data<Number, String> item, int itemIndex) {

        Node container = item.getNode();

        if (container == null) {
            container = new StackPane();
            item.setNode(container);
        }

        // TODO: nie wybierania kolorów jeszcze. Wszystko jest różowe
        /*
        Color style = ((ExtraData) item.getExtraValue()).getStyle();
        int red = (int) (style.getRed() * 255);
        int green = (int) (style.getGreen() * 255);
        int blue = (int) (style.getBlue() * 255);
        String cssValue = String.format("-fx-background-color: rgba(%d, %d, %d, %f)", red, green, blue, style.getOpacity());
         */

        String cssValue = "-fx-background-color: rgba(255, 105, 180, 1.0)";
        container.setStyle(cssValue);

        return container;
    }

    @Override
    protected void updateAxisRange() {
        final Axis<Number> xa = getXAxis();
        final Axis<String> ya = getYAxis();
        List<Number> xData = null;
        List<String> yData = null;
        if (xa.isAutoRanging()) xData = new ArrayList<Number>();
        if (ya.isAutoRanging()) yData = new ArrayList<String>();
        if (xData != null || yData != null) {
            for (Series<Number, String> series : getData()) {
                for (Data<Number, String> data : series.getData()) {
                    if (xData != null) {
                        xData.add(data.getXValue());
                        xData.add(xa.toRealValue(xa.toNumericValue(data.getXValue()) + ((ExtraData) data.getExtraValue()).getLength()));
                    }
                    if (yData != null) {
                        yData.add(data.getYValue());
                    }
                }
            }
            if (xData != null) xa.invalidateRange(xData);
            if (yData != null) ya.invalidateRange(yData);
        }
    }

}