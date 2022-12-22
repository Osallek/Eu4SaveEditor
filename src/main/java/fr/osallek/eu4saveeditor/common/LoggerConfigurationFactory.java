package fr.osallek.eu4saveeditor.common;

import java.io.File;
import java.net.URI;
import java.time.LocalDate;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.appender.ConsoleAppender;
import org.apache.logging.log4j.core.config.Configuration;
import org.apache.logging.log4j.core.config.ConfigurationFactory;
import org.apache.logging.log4j.core.config.ConfigurationSource;
import org.apache.logging.log4j.core.config.Order;
import org.apache.logging.log4j.core.config.builder.api.AppenderComponentBuilder;
import org.apache.logging.log4j.core.config.builder.api.ConfigurationBuilder;
import org.apache.logging.log4j.core.config.builder.api.LayoutComponentBuilder;
import org.apache.logging.log4j.core.config.builder.api.RootLoggerComponentBuilder;
import org.apache.logging.log4j.core.config.builder.impl.BuiltConfiguration;
import org.apache.logging.log4j.core.config.plugins.Plugin;

@Order(50)
@Plugin(name = "LoggerConfigurationFactory", category = ConfigurationFactory.CATEGORY)
public class LoggerConfigurationFactory extends ConfigurationFactory {

    @Override
    public Configuration getConfiguration(final LoggerContext loggerContext, final ConfigurationSource source) {
        return getConfiguration(loggerContext, source.toString(), null);
    }

    @Override
    public Configuration getConfiguration(final LoggerContext loggerContext, final String name, final URI configLocation) {
        ConfigurationBuilder<BuiltConfiguration> builder = newConfigurationBuilder();
        RootLoggerComponentBuilder rootLogger = builder.newRootLogger(Level.INFO);

        builder.setStatusLevel(Level.INFO);
        builder.setConfigurationName("DefaultFileLogger");

        AppenderComponentBuilder appenderBuilder = builder.newAppender("Console", "CONSOLE")
                                                          .addAttribute("target", ConsoleAppender.Target.SYSTEM_OUT);
        appenderBuilder.add(builder.newLayout("PatternLayout")
                                   .addAttribute("pattern",
                                                 "%d{HH:mm:ss.SSS} %style{[%t]}{magenta} %highlight{%-5level}{ERROR=red, WARN=yellow, INFO=blue, DEBUG=magenta, TRACE=white} %logger{36} - %msg%n")
                                   .addAttribute("disableAnsi", false));
        rootLogger.add(builder.newAppenderRef("Console"));

        LayoutComponentBuilder layoutBuilder = builder.newLayout("PatternLayout")
                                                      .addAttribute("pattern", "%d{HH:mm:ss.SSS} %-5level %logger{36} - %msg%n");
        AppenderComponentBuilder fileAppenderBuilder = builder.newAppender("File", "File")
                                                              .addAttribute("fileName",
                                                                            Constants.EDITOR_FOLDER + File.separator + "logs" + File.separator
                                                                            + "Eu4SaveEditor-" + LocalDate.now().toString() + ".log")
                                                              .addAttribute("ignoreExceptions", false)
                                                              .addAttribute("append", true)
                                                              .add(layoutBuilder);

        rootLogger.add(builder.newAppenderRef("Console"));
        rootLogger.add(builder.newAppenderRef("File"));

        builder.add(appenderBuilder);
        builder.add(fileAppenderBuilder);
        builder.add(rootLogger);

        return builder.build();
    }

    @Override
    public String[] getSupportedTypes() {
        return new String[] {"*"};
    }
}

