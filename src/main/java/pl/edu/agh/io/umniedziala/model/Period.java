package pl.edu.agh.io.umniedziala.model;

public abstract class Period {
    protected int id;
    protected String startTime;
    protected String endTime;

    public int getId() {
        return id;
    }
    public String getStartTime() {
        return startTime;
    }
    public String getEndTime() {
        return endTime;
    }
    public abstract String getColor();
}
