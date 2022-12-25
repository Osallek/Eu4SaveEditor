package fr.osallek.eu4saveeditor.controller.mapview;

import fr.osallek.clausewitzparser.common.ClausewitzUtils;
import fr.osallek.eu4parser.model.game.Culture;
import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.SaveReligion;
import fr.osallek.eu4parser.model.save.country.Heir;
import fr.osallek.eu4parser.model.save.country.Monarch;
import fr.osallek.eu4parser.model.save.country.SaveCountry;
import fr.osallek.eu4saveeditor.Eu4SaveEditorApplication;
import fr.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import fr.osallek.eu4saveeditor.controller.control.ClearableSpinnerInt;
import fr.osallek.eu4saveeditor.controller.control.TableView2Personalities;
import fr.osallek.eu4saveeditor.controller.converter.CultureStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.CultureStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.SaveReligionStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.SaveReligionStringConverter;
import fr.osallek.eu4saveeditor.controller.object.Personality;
import fr.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import fr.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import fr.osallek.eu4saveeditor.controller.pane.TableViewDialog;
import fr.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ButtonItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableDatePickerItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSpinnerItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import fr.osallek.eu4saveeditor.controller.validator.CustomGraphicValidationDecoration;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.layout.VBox;
import org.apache.commons.collections4.CollectionUtils;
import org.controlsfx.control.SearchableComboBox;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;

public class MonarchPropertySheet extends VBox {

    private final SaveCountry country;

    private final Monarch monarch;

    private final CustomPropertySheet propertySheet;

    private final ClearableTextItem nameField;

    private final ClearableSpinnerItem<Integer> admPointField;

    private final ClearableSpinnerItem<Integer> dipPointField;

    private final ClearableSpinnerItem<Integer> milPointField;

    private final ClearableComboBoxItem<Culture> cultureField;

    private final ClearableComboBoxItem<SaveReligion> religionField;

    private final ClearableDatePickerItem birthDateField;

    private final ClearableSliderItem claimField;

    private final ObservableList<Personality> personalities = FXCollections.observableArrayList();

    private final ValidationSupport validationSupport;

    public MonarchPropertySheet(SaveCountry country, Monarch monarch, String name, ObservableList<Culture> cultures, ObservableList<SaveReligion> religions) {
        this.country = country;
        this.monarch = monarch;
        this.propertySheet = new CustomPropertySheet();
        this.propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.propertySheet.setMode(CustomPropertySheet.Mode.CATEGORY);
        this.propertySheet.setModeSwitcherVisible(false);
        this.propertySheet.setSearchBoxVisible(false);

        List<CustomPropertySheet.Item> items = new ArrayList<>();

        CustomPropertySheetSkin propertySheetSkin = new CustomPropertySheetSkin(this.propertySheet);
        this.propertySheet.setSkin(propertySheetSkin);

        this.nameField = new ClearableTextItem(name, this.country.getSave().getGame().getLocalisationClean("LEDGER_NAME", Eu4Language.getDefault()));
        this.nameField.getTextField().getStylesheets().add(Eu4SaveEditorApplication.class.getResource("/styles/style.css").toExternalForm());
        this.nameField.setValue(ClausewitzUtils.removeQuotes(this.monarch.getName()));
        this.nameField.setSupplier(() -> ClausewitzUtils.removeQuotes(this.monarch.getName()));
        items.add(this.nameField);

        this.cultureField = new ClearableComboBoxItem<>(name,
                                                        this.country.getSave().getGame().getLocalisationClean("LEDGER_CULTURE", Eu4Language.getDefault()),
                                                        cultures,
                                                        new ClearableComboBox<>(new SearchableComboBox<>()));
        this.cultureField.setConverter(CultureStringConverter.INSTANCE);
        this.cultureField.setCellFactory(CultureStringCellFactory.INSTANCE);
        this.cultureField.setValue(this.monarch.getCulture());
        this.cultureField.setSupplier(this.monarch::getCulture);
        items.add(this.cultureField);

        this.religionField = new ClearableComboBoxItem<>(name,
                                                         this.country.getSave().getGame().getLocalisationClean("LEDGER_RELIGION", Eu4Language.getDefault()),
                                                         religions,
                                                         new ClearableComboBox<>(new SearchableComboBox<>()));
        this.religionField.setConverter(SaveReligionStringConverter.INSTANCE);
        this.religionField.setCellFactory(SaveReligionStringCellFactory.INSTANCE);
        this.religionField.setValue(this.monarch.getReligion());
        this.religionField.setSupplier(this.monarch::getReligion);
        items.add(this.religionField);

        this.admPointField = new ClearableSpinnerItem<>(name,
                                                        this.country.getSave()
                                                                    .getGame()
                                                                    .getLocalisationCleanNoPunctuation("COURT_ADM", Eu4Language.getDefault()),
                                                        new ClearableSpinnerInt(this.country.getSave().getGame().getMonarchMinSkill(),
                                                                                this.country.getSave().getGame().getMonarchMaxSkill(),
                                                                                this.monarch.getAdm(), 1, this.monarch::getAdm));
        items.add(this.admPointField);

        this.dipPointField = new ClearableSpinnerItem<>(name,
                                                        this.country.getSave()
                                                                    .getGame()
                                                                    .getLocalisationCleanNoPunctuation("COURT_DIP", Eu4Language.getDefault()),
                                                        new ClearableSpinnerInt(this.country.getSave().getGame().getMonarchMinSkill(),
                                                                                this.country.getSave().getGame().getMonarchMaxSkill(),
                                                                                this.monarch.getDip(), 1, this.monarch::getDip));
        items.add(this.dipPointField);

        this.milPointField = new ClearableSpinnerItem<>(name,
                                                        this.country.getSave()
                                                                    .getGame()
                                                                    .getLocalisationCleanNoPunctuation("COURT_MIL", Eu4Language.getDefault()),
                                                        new ClearableSpinnerInt(this.country.getSave().getGame().getMonarchMinSkill(),
                                                                                this.country.getSave().getGame().getMonarchMaxSkill(),
                                                                                this.monarch.getMil(), 1, this.monarch::getMil));
        items.add(this.milPointField);

        if (ChronoUnit.YEARS.between(this.monarch.getBirthDate(), this.country.getSave().getDate()) >= this.country.getSave().getGame().getAgeOfAdulthood()) {
            this.birthDateField = new ClearableDatePickerItem(name, this.country.getSave()
                                                                                .getGame()
                                                                                .getLocalisationClean("DATE_OF_BIRTH_REQUIRED", Eu4Language.getDefault()),
                                                              this.monarch.getBirthDate(), this.monarch::getBirthDate,
                                                              null,
                                                              this.country.getSave()
                                                                          .getDate()
                                                                          .minusYears(this.country.getSave().getGame().getAgeOfAdulthood()));
        } else {
            this.birthDateField = new ClearableDatePickerItem(name, this.country.getSave()
                                                                                .getGame()
                                                                                .getLocalisationClean("DATE_OF_BIRTH_REQUIRED", Eu4Language.getDefault()),
                                                              this.monarch.getBirthDate(), this.monarch::getBirthDate,
                                                              this.country.getSave()
                                                                          .getDate()
                                                                          .minusYears(this.country.getSave().getGame().getAgeOfAdulthood())
                                                                          .plusDays(1), this.country.getSave().getDate());
        }
        items.add(this.birthDateField);

        if (Heir.class.equals(this.monarch.getClass()) && ((Heir) this.monarch).getClaim() != null) {
            this.claimField = new ClearableSliderItem(name, this.country.getSave()
                                                                        .getGame()
                                                                        .getLocalisationClean("legitimacy", Eu4Language.getDefault()), 0, 100,
                                                      ((Heir) this.monarch).getClaim(), ((Heir) this.monarch)::getClaim);
            items.add(this.claimField);
        } else {
            this.claimField = null;
        }

        this.personalities.setAll(this.monarch.getPersonalities() == null ? new ArrayList<>() : this.monarch.getPersonalities()
                                                                                                            .getPersonalities()
                                                                                                            .stream()
                                                                                                            .map(Personality::new)
                                                                                                            .collect(Collectors.toList()));
        ButtonItem personalitiesButton = new ButtonItem(name, null, this.country.getSave()
                                                                                .getGame()
                                                                                .getLocalisationClean("LEDGER_PERSONALITIES", Eu4Language.getDefault()), 2);
        personalitiesButton.getButton().setOnAction(event -> {
            TableView2Personalities view2Personalities = new TableView2Personalities(this.country, this.monarch, this.personalities,
                                                                                     this.country.getSave()
                                                                                                 .getGame()
                                                                                                 .getRulerPersonalities()
                                                                                                 .stream()
                                                                                                 .map(Personality::new)
                                                                                                 .collect(Collectors.toCollection(FXCollections::observableArrayList)));
            TableViewDialog<Personality> dialog =
                    new TableViewDialog<>(this.country.getSave(),
                                          view2Personalities,
                                          this.country.getSave().getGame().getLocalisationClean("LEDGER_PERSONALITIES", Eu4Language.getDefault()),
                                          list -> this.country.getSave()
                                                              .getGame()
                                                              .getRulerPersonalities()
                                                              .stream()
                                                              .map(Personality::new)
                                                              .filter(personality -> list.stream().noneMatch(i -> i.equals(personality)))
                                                              .filter(personality -> personality.getRulerPersonality().isMonarchValid(monarch))
                                                              .findFirst()
                                                              .get(),
                                          () -> this.personalities);
            dialog.setDisableAddProperty(view2Personalities.disableAddPropertyProperty());
            Optional<List<Personality>> rulerPersonalities = dialog.showAndWait();

            rulerPersonalities.ifPresent(this.personalities::setAll);
        });
        items.add(personalitiesButton);

        this.validationSupport = new ValidationSupport();
        this.validationSupport.setValidationDecorator(new CompoundValidationDecoration(new CustomGraphicValidationDecoration(),
                                                                                       new StyleClassValidationDecoration("validation-error", null)));

        this.propertySheet.getItems().setAll(items);
    }

    public void validate() {
        if (!ClausewitzUtils.removeQuotes(this.monarch.getName()).equals(this.nameField.getText())) {
            this.monarch.setName(ClausewitzUtils.addQuotes(this.nameField.getText()));
        }

        if (!Objects.deepEquals(this.monarch.getCulture(), this.cultureField.getSelectedValue())) {
            this.monarch.setCulture(this.cultureField.getSelectedValue());
        }

        if (!Objects.deepEquals(this.monarch.getReligion(), this.religionField.getSelectedValue())) {
            this.monarch.setReligion(this.religionField.getSelectedValue());
        }

        if (!Objects.deepEquals(this.monarch.getAdm(), this.admPointField.getTrueValue())) {
            this.monarch.setAdm(this.admPointField.getTrueValue());
        }

        if (!Objects.deepEquals(this.monarch.getDip(), this.dipPointField.getTrueValue())) {
            this.monarch.setDip(this.dipPointField.getTrueValue());
        }

        if (!Objects.deepEquals(this.monarch.getMil(), this.milPointField.getTrueValue())) {
            this.monarch.setMil(this.milPointField.getTrueValue());
        }

        if (!Objects.deepEquals(this.monarch.getBirthDate(), this.birthDateField.getTrueValue())) {
            this.monarch.setBirthDate(this.birthDateField.getTrueValue());
        }

        if (this.claimField != null && !Objects.deepEquals(((Heir) this.monarch).getClaim(), this.claimField.getDoubleValue())) {
            ((Heir) this.monarch).setClaim(this.claimField.getDoubleValue());
        }

        if ((this.monarch.getPersonalities() == null && CollectionUtils.isNotEmpty(this.personalities))
            || (this.monarch.getPersonalities() != null
                && !Objects.equals(this.monarch.getPersonalities()
                                               .getPersonalities()
                                               .stream()
                                               .map(Personality::new)
                                               .collect(Collectors.toList()), this.personalities))) {
            if (this.monarch.getPersonalities() != null) {
                this.monarch.getPersonalities()
                            .getPersonalities()
                            .forEach(personality -> this.personalities.stream()
                                                                      .filter(p -> p.getRulerPersonality().equals(personality))
                                                                      .findFirst()
                                                                      .ifPresentOrElse(this.personalities::remove,
                                                                                       () -> this.monarch.removePersonality(personality)));
            }

            this.personalities.stream().map(Personality::getRulerPersonality).forEach(this.monarch::addPersonality);
        }
    }

    public Monarch getMonarch() {
        return monarch;
    }

    public ValidationSupport getValidationSupport() {
        return validationSupport;
    }

    public CustomPropertySheet getPropertySheet() {
        return propertySheet;
    }
}
