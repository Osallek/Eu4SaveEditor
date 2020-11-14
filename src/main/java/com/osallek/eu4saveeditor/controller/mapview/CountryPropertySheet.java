package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.common.Eu4Utils;
import com.osallek.eu4parser.model.Power;
import com.osallek.eu4parser.model.game.Culture;
import com.osallek.eu4parser.model.game.GovernmentReform;
import com.osallek.eu4parser.model.game.SubjectType;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.SaveReligion;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import com.osallek.eu4saveeditor.controller.control.ClearableSpinnerInt;
import com.osallek.eu4saveeditor.controller.control.RequiredComboBox;
import com.osallek.eu4saveeditor.controller.control.TableView2CountrySubject;
import com.osallek.eu4saveeditor.controller.converter.PairCellFactory;
import com.osallek.eu4saveeditor.controller.converter.PairConverter;
import com.osallek.eu4saveeditor.controller.object.CountrySubject;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import com.osallek.eu4saveeditor.controller.pane.GovernmentReformsDialog;
import com.osallek.eu4saveeditor.controller.pane.TableViewDialog;
import com.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ButtonItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.CheckBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSpinnerItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import com.osallek.eu4saveeditor.controller.validator.CustomGraphicValidationDecoration;
import com.osallek.eu4saveeditor.i18n.SheetCategory;
import com.sun.javafx.collections.ObservableListWrapper;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.ComboBox;
import javafx.scene.layout.VBox;
import org.apache.commons.lang3.tuple.Pair;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.Validator;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CountryPropertySheet extends VBox {

    private static final Logger LOGGER = LoggerFactory.getLogger(CountryPropertySheet.class);

    private Country country;

    private final ObservableList<Country> countriesAlive;

    private final ObservableList<SubjectType> subjectTypes;

    private final CustomPropertySheet propertySheet;

    private final ValidationSupport validationSupport;

    private final ClearableTextItem nameField;

    private final CheckBoxItem wasPlayerField;

    private final ClearableSpinnerItem<Integer> admPointField;

    private final ClearableSpinnerItem<Integer> dipPointField;

    private final ClearableSpinnerItem<Integer> milPointField;

    private final ClearableComboBoxItem<Pair<String, String>> governmentRankField;

    private final ButtonItem governmentReformsButton;

    private final ObservableList<GovernmentReform> governmentReformsField;

    private final ButtonItem countrySubjectsButton;

    private Map<Country, CountrySubject> countrySubjectsField;

    private final ClearableComboBoxItem<Country> overlordField;

    private final CustomPropertySheetSkin propertySheetSkin;

    public CountryPropertySheet(Save save, ObservableList<Country> countriesAlive, ObservableList<Culture> cultures, ObservableList<SaveReligion> religions) {
        this.countriesAlive = countriesAlive;
        this.subjectTypes = new ObservableListWrapper<>(save.getGame()
                                                            .getSubjectTypes()
                                                            .stream()
                                                            .filter(type -> !"default".equals(type.getName()))
                                                            .sorted(Comparator.comparing(SubjectType::getLocalizedName, Eu4Utils.COLLATOR))
                                                            .collect(Collectors.toList()));
        this.propertySheet = new CustomPropertySheet();
        this.propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.propertySheet.setMode(CustomPropertySheet.Mode.CATEGORY);
        this.propertySheet.setModeSwitcherVisible(false);
        this.propertySheet.setSearchBoxVisible(false);

        this.propertySheetSkin = new CustomPropertySheetSkin(this.propertySheet);
        this.propertySheet.setSkin(this.propertySheetSkin);

        //GENERAL
        this.nameField = new ClearableTextItem(SheetCategory.GENERAL, save.getGame().getLocalisation("LEDGER_NAME"));
        this.nameField.getTextField()
                      .getStylesheets()
                      .add(getClass().getClassLoader().getResource("styles/propertySheetsStyle.css").toExternalForm());

        this.wasPlayerField = new CheckBoxItem(SheetCategory.GENERAL, save.getGame().getLocalisationClean("WAS_PLAYER"), false);

        this.admPointField = new ClearableSpinnerItem<>(SheetCategory.GENERAL,
                                                        save.getGame().getLocalisationClean("ADM_POWER"),
                                                        new ClearableSpinnerInt(0, 999, 1));

        this.dipPointField = new ClearableSpinnerItem<>(SheetCategory.GENERAL,
                                                        save.getGame().getLocalisationClean("DIP_POWER"),
                                                        new ClearableSpinnerInt(0, 999, 1));

        this.milPointField = new ClearableSpinnerItem<>(SheetCategory.GENERAL,
                                                        save.getGame().getLocalisationClean("MIL_POWER"),
                                                        new ClearableSpinnerInt(0, 999, 1));

        //LEDGER_GOVERNMENT_NAME
        this.governmentRankField = new ClearableComboBoxItem<>(SheetCategory.COUNTRY_GOVERNMENT,
                                                               save.getGame().getLocalisation("GOV_RANK"),
                                                               FXCollections.observableArrayList(),
                                                               new ClearableComboBox<>(new RequiredComboBox<>()));
        this.governmentRankField.setConverter(new PairConverter());
        this.governmentRankField.setCellFactory(new PairCellFactory());

        this.governmentReformsButton = new ButtonItem(SheetCategory.COUNTRY_GOVERNMENT,
                                                      null,
                                                      save.getGame().getLocalisationClean("governmental_reforms"),
                                                      2);
        this.governmentReformsField = new ObservableListWrapper<>(new ArrayList<>());

        this.countrySubjectsButton = new ButtonItem(save.getGame().getLocalisationClean("HEADER_SUBJECTS"),
                                                    null,
                                                    save.getGame().getLocalisationClean("HEADER_SUBJECTS"),
                                                    2);

        this.overlordField = new ClearableComboBoxItem<>(save.getGame().getLocalisationClean("HEADER_SUBJECTS"),
                                                         save.getGame().getLocalisation("HEADER_OVERLORD"),
                                                         FXCollections.observableArrayList(new ArrayList<>()),
                                                         new ClearableComboBox<>(new ComboBox<>()));
        this.overlordField.setEditable(false);

        this.validationSupport = new ValidationSupport();
        this.validationSupport.registerValidator(this.nameField.getTextField(), Validator.createEmptyValidator("Text is required"));
        this.validationSupport.setValidationDecorator(new CompoundValidationDecoration(new CustomGraphicValidationDecoration(),
                                                                                       new StyleClassValidationDecoration("validation-error", null)));
    }

    public void update(Country country) {
        update(country, false);
    }

    public void update(Country country, boolean force) {
        if (force || this.country == null || !this.country.equals(country)) {
            this.country = country;

            if (this.country == null) {
                this.propertySheet.getItems().clear();
            } else {
                String expandedPaneName = this.propertySheetSkin.getAccordion().getExpandedPane() == null ? null :
                                          this.propertySheetSkin.getAccordion().getExpandedPane().getText();

                List<CustomPropertySheet.Item> items = new ArrayList<>();

                //GENERAL
                this.nameField.setValue(ClausewitzUtils.removeQuotes(this.country.getLocalizedName()));
                this.nameField.setSupplier(() -> ClausewitzUtils.removeQuotes(this.country.getLocalizedName()));
                this.nameField.setEditable(this.country.isNameEditable());
                items.add(this.nameField);

                this.wasPlayerField.setValue(this.country.wasPlayer());
                items.add(this.wasPlayerField);

                this.admPointField.setSupplier(() -> this.country.getPowers().get(Power.ADM));
                this.admPointField.setMax(Math.max(this.country.getPowers().get(Power.ADM), 999));
                this.admPointField.setValue(this.country.getPowers().get(Power.ADM));
                items.add(this.admPointField);

                this.dipPointField.setSupplier(() -> this.country.getPowers().get(Power.DIP));
                this.dipPointField.setMax(Math.max(this.country.getPowers().get(Power.DIP), 999));
                this.dipPointField.setValue(this.country.getPowers().get(Power.DIP));
                items.add(this.dipPointField);

                this.milPointField.setSupplier(() -> this.country.getPowers().get(Power.MIL));
                this.milPointField.setMax(Math.max(this.country.getPowers().get(Power.MIL), 999));
                this.milPointField.setValue(this.country.getPowers().get(Power.MIL));
                items.add(this.milPointField);

                //Government
                this.governmentRankField.getChoices().setAll(this.country.getGovernmentName().getRanks().values());
                this.governmentRankField.setValue(this.country.getGovernmentName().getRank(this.country.getGovernmentLevel()));
                this.governmentRankField.setSupplier(() -> this.country.getGovernmentName().getRank(this.country.getGovernmentLevel()));
                items.add(this.governmentRankField);

                this.governmentReformsField.setAll(this.country.getGovernment().getReforms());
                this.governmentReformsButton.getButton().setOnAction(event -> {
                    GovernmentReformsDialog dialog = new GovernmentReformsDialog(this.country, this.governmentReformsField);

                    Optional<List<GovernmentReform>> reforms = dialog.showAndWait();

                    reforms.ifPresent(this.governmentReformsField::setAll);
                });
                items.add(this.governmentReformsButton);

                //Subjects
                this.overlordField.setValues(FXCollections.observableArrayList(this.country.getOverlord()));
                this.overlordField.setValue(this.country.getOverlord());
                items.add(this.overlordField);

                this.countrySubjectsField = this.country.getSubjects()
                                                        .stream()
                                                        .map(CountrySubject::new)
                                                        .collect(Collectors.toMap(CountrySubject::getSubject, Function.identity()));
                this.countrySubjectsButton.getButton().setOnAction(event -> {
                    TableViewDialog<CountrySubject> dialog =
                            new TableViewDialog<>(this.country.getSave(),
                                                  new TableView2CountrySubject(this.country,
                                                                               this.countrySubjectsField.values()
                                                                                                        .stream()
                                                                                                        .map(CountrySubject::new)
                                                                                                        .collect(Collectors.toCollection(
                                                                                                                FXCollections::observableArrayList)),
                                                                               this.countriesAlive,
                                                                               this.subjectTypes),
                                                  this.country.getSave().getGame().getLocalisationClean("HEADER_SUBJECTS"),
                                                  () -> new CountrySubject(this.country,
                                                                           this.countriesAlive.get(0),
                                                                           this.subjectTypes.get(0),
                                                                           this.country.getSave().getDate()),
                                                  () -> this.country.getSubjects()
                                                                    .stream()
                                                                    .map(CountrySubject::new)
                                                                    .collect(Collectors.toList()));
                    Optional<List<CountrySubject>> countrySubjects = dialog.showAndWait();

                    countrySubjects.ifPresent(list -> this.countrySubjectsField = list.stream()
                                                                                      .map(CountrySubject::new)
                                                                                      .collect(Collectors.toMap(CountrySubject::getSubject,
                                                                                                                Function.identity())));
                });
                items.add(this.countrySubjectsButton);

                this.propertySheet.getItems().setAll(items);

                if (expandedPaneName != null) {
                    this.propertySheetSkin.getAccordion()
                                          .getPanes()
                                          .stream()
                                          .filter(titledPane -> titledPane.getText().equals(expandedPaneName))
                                          .findFirst()
                                          .ifPresent(titledPane -> this.propertySheetSkin.getAccordion().setExpandedPane(titledPane));
                }
            }
        }
    }

    public void validate(ActionEvent actionEvent) {
        if (!ClausewitzUtils.removeQuotes(this.country.getLocalizedName()).equals(this.nameField.getText())) {
            this.country.setLocalizedName(this.nameField.getText());
        }

        if (!Objects.equals(this.country.wasPlayer(), this.wasPlayerField.isSelected())) {
            this.country.setWasPlayer(this.wasPlayerField.isSelected());
        }

        if (!Objects.equals(this.country.getGovernmentName().getRank(this.country.getGovernmentLevel()), this.governmentRankField.getSelectedValue())) {
            this.country.setGovernmentRank(this.governmentRankField.getSelectedValue().getKey());
        }

        if (!Objects.equals(this.country.getGovernment().getReforms(), this.governmentReformsField)) {
            this.country.getGovernment().setReforms(this.governmentReformsField);
        }

        if (!Objects.equals(this.country.getPowers().get(Power.ADM), this.admPointField.getTrueValue())) {
            this.country.setPower(Power.ADM, this.admPointField.getTrueValue());
        }

        if (!Objects.equals(this.country.getPowers().get(Power.DIP), this.dipPointField.getTrueValue())) {
            this.country.setPower(Power.DIP, this.dipPointField.getTrueValue());
        }

        if (!Objects.equals(this.country.getPowers().get(Power.MIL), this.milPointField.getTrueValue())) {
            this.country.setPower(Power.MIL, this.milPointField.getTrueValue());
        }

        Stream.concat(this.country.getSubjects().stream(), this.countrySubjectsField.keySet().stream())
              .distinct()
              .forEach(subject -> this.countrySubjectsField.compute(subject, (c, countrySubject) -> {
                  if (countrySubject == null) {
                      this.country.getSave().getDiplomacy().removeDependency(this.country, subject);
                  } else if (countrySubject.changed()) {
                      subject.setOverlord(countrySubject.getOverlord(), countrySubject.getSubjectType(),
                                          countrySubject.getStartDate());
                  }

                  return countrySubject;
              }));

        update(this.country, true);
    }

    public Country getCountry() {
        return country;
    }

    public ValidationSupport getValidationSupport() {
        return validationSupport;
    }

    public CustomPropertySheet getPropertySheet() {
        return propertySheet;
    }
}
