package pl.edu.agh.io.umniedziala.viewController;

import pl.edu.agh.io.umniedziala.model.ApplicationEntity;

import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;

public class ManagingApplicationsController {

    private static final Logger logger = Logger.getLogger(ManagingApplicationsController.class.getName());

    public boolean addNewApplicationByPath(String path, int color) {
        if (path.length() <= 0) {
            logger.log(Level.WARNING, "Empty path.");

            return false;
        } else {
            String appName = path.split("\\\\")[path.split("\\\\").length - 1];

            ApplicationEntity.create(appName, path, color);
            logger.info(String.format("Added %s with path: %s", appName, path));

            return true;
        }
    }

    public boolean deleteApplication(String path) {
        if (path.length() <= 0) {
            logger.log(Level.WARNING, "Empty path.");

            return false;
        } else {
            Optional<ApplicationEntity> application = ApplicationEntity.findByApplicationPath(path);

            if (!application.isPresent()) {
                logger.log(Level.WARNING, String.format("Application: %s not observed", path));
                return false;
            }

            return true;
        }
    }
}
