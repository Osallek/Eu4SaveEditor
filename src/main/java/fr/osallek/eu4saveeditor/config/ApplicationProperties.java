package fr.osallek.eu4saveeditor.config;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConfigurationProperties(prefix = "application")
public class ApplicationProperties {

    private ArtifactVersion version;

    private String gameVersion;

    public ArtifactVersion getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = new DefaultArtifactVersion(version);
    }

    public String getGameVersion() {
        return gameVersion;
    }

    public void setGameVersion(String gameVersion) {
        this.gameVersion = gameVersion;
    }
}
