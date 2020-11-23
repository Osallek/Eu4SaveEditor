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
import com.osallek.eu4saveeditor.controller.control.ClearableCheckComboBox;
import com.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import com.osallek.eu4saveeditor.controller.control.ClearableSpinnerDouble;
import com.osallek.eu4saveeditor.controller.control.ClearableSpinnerInt;
import com.osallek.eu4saveeditor.controller.control.RequiredComboBox;
import com.osallek.eu4saveeditor.controller.control.TableView2CountrySubject;
import com.osallek.eu4saveeditor.controller.control.TableView2Ideas;
import com.osallek.eu4saveeditor.controller.control.TableView2Loan;
import com.osallek.eu4saveeditor.controller.converter.CultureStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.CultureStringConverter;
import com.osallek.eu4saveeditor.controller.converter.PairCellFactory;
import com.osallek.eu4saveeditor.controller.converter.PairConverter;
import com.osallek.eu4saveeditor.controller.object.CountrySubject;
import com.osallek.eu4saveeditor.controller.object.Idea;
import com.osallek.eu4saveeditor.controller.object.Loan;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import com.osallek.eu4saveeditor.controller.pane.GovernmentReformsDialog;
import com.osallek.eu4saveeditor.controller.pane.TableViewDialog;
import com.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ButtonItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.CheckBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableCheckComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderIntItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSpinnerItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.PropertySheetItem;
import com.osallek.eu4saveeditor.controller.validator.CustomGraphicValidationDecoration;
import com.osallek.eu4saveeditor.i18n.SheetCategory;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.ComboBox;
import javafx.scene.layout.VBox;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.controlsfx.control.SearchableComboBox;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.Validator;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
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

    private final ObservableList<Culture> cultures;

    private final ObservableList<SaveReligion> religions;

    private final ObservableList<Country> countriesAlive;

    private final ObservableList<SubjectType> subjectTypes;

    private final CustomPropertySheet propertySheet;

    private final ValidationSupport validationSupport;

    private final ClearableTextItem nameField;

    private final CheckBoxItem wasPlayerField;

    private final ClearableSpinnerItem<Integer> admPointField;

    private final ClearableSpinnerItem<Integer> dipPointField;

    private final ClearableSpinnerItem<Integer> milPointField;

    private final ClearableSpinnerItem<Integer> stabilityField;

    private final ClearableSpinnerItem<Double> prestigeField;

    private final ClearableSpinnerItem<Integer> absolutismField;

    private final ClearableComboBoxItem<Culture> cultureField;

    private final ClearableCheckComboBoxItem<Culture> acceptedCulturesField;

    private final ClearableSpinnerItem<Double> governmentReformProgressField;

    private final ClearableComboBoxItem<Pair<String, String>> governmentRankField;

    private final ButtonItem governmentReformsButton;

    private final ObservableList<GovernmentReform> governmentReformsField;

    private final CustomPropertySheet courtPropertySheet;

    private CustomPropertySheetSkin courtPropertySheetSkin;

    private MonarchPropertySheet monarchPropertySheet;

    private MonarchPropertySheet heirPropertySheet;

    private MonarchPropertySheet queenPropertySheet;

    private final ButtonItem countrySubjectsButton;

    private Map<Country, CountrySubject> countrySubjectsField;

    private final ClearableComboBoxItem<Country> overlordField;

    private final ClearableSpinnerItem<Double> treasuryField;

    private final ClearableSpinnerItem<Double> inflationField;

    private final ClearableSliderItem corruptionField;

    private final ClearableSliderIntItem mercantilismField;

    private final ClearableSpinnerItem<Double> manpowerField;

    private final ClearableSpinnerItem<Double> sailorsField;

    private final ClearableSliderItem armyTraditionField;

    private final ClearableSliderItem navyTraditionField;

    private final ClearableSliderItem armyProfessionalismField;

    private final ClearableSliderItem warEhaustionField;

    private final ButtonItem loansButton;

    private final ObservableList<Loan> loans;

    private final CustomPropertySheet estatesPropertySheet;

    private final List<EstatePropertySheet> estatePropertySheets;

    private CustomPropertySheetSkin estatesPropertySheetSkin;

    private final ClearableSpinnerItem<Integer> admTechField;

    private final ClearableSpinnerItem<Integer> dipTechField;

    private final ClearableSpinnerItem<Integer> milTechField;

    private final ClearableSliderItem innovativenessField;

    private final ButtonItem ideasButton;

    private final ObservableList<Idea> ideas;

    private final CustomPropertySheetSkin propertySheetSkin;

    public CountryPropertySheet(Save save, ObservableList<Country> countriesAlive, ObservableList<Culture> cultures, ObservableList<SaveReligion> religions) {
        this.countriesAlive = countriesAlive;
        this.cultures = cultures;
        this.religions = religions;
        this.subjectTypes = save.getGame()
                                .getSubjectTypes()
                                .stream()
                                .filter(type -> !"default".equals(type.getName()))
                                .sorted(Comparator.comparing(SubjectType::getLocalizedName, Eu4Utils.COLLATOR))
                                .collect(Collectors.toCollection(FXCollections::observableArrayList));
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

        this.stabilityField = new ClearableSpinnerItem<>(SheetCategory.GENERAL,
                                                         save.getGame().getLocalisationClean("stability"),
                                                         new ClearableSpinnerInt(-3, 3, 1));

        this.prestigeField = new ClearableSpinnerItem<>(SheetCategory.GENERAL,
                                                        save.getGame().getLocalisationClean("prestige"),
                                                        new ClearableSpinnerDouble(-100, 100, 1));

        this.absolutismField = new ClearableSpinnerItem<>(SheetCategory.GENERAL,
                                                          save.getGame().getLocalisationClean("absolutism"),
                                                          new ClearableSpinnerInt(0, 100, 1));

        //Culture
        this.cultureField = new ClearableComboBoxItem<>(save.getGame().getLocalisation("LEDGER_CULTURE"),
                                                        save.getGame().getLocalisation("LEDGER_CULTURE"),
                                                        cultures,
                                                        new ClearableComboBox<>(new SearchableComboBox<>()));
        this.cultureField.setConverter(new CultureStringConverter());
        this.cultureField.setCellFactory(new CultureStringCellFactory());

        this.acceptedCulturesField = new ClearableCheckComboBoxItem<>(save.getGame().getLocalisation("LEDGER_CULTURE"),
                                                                      save.getGame().getLocalisation("MAPMODE_ACCEPTEDCULTURES"),
                                                                      cultures,
                                                                      new ClearableCheckComboBox<>());
        this.acceptedCulturesField.setConverter(new CultureStringConverter());

        //Economy
        this.treasuryField = new ClearableSpinnerItem<>(SheetCategory.ECONOMY,
                                                        save.getGame().getLocalisationClean("TECH_TRESURY_TITLE"),
                                                        new ClearableSpinnerDouble(-1000000, 1000000, 100));

        this.inflationField = new ClearableSpinnerItem<>(SheetCategory.ECONOMY,
                                                         save.getGame().getLocalisationClean("inflation"),
                                                         new ClearableSpinnerDouble(0, Integer.MAX_VALUE / 1000, 1));

        this.corruptionField = new ClearableSliderItem(SheetCategory.ECONOMY,
                                                       save.getGame().getLocalisation("corruption"),
                                                       0, 100);

        this.mercantilismField = new ClearableSliderIntItem(SheetCategory.ECONOMY,
                                                            save.getGame().getLocalisation("MERCANTILISM_LABEL"),
                                                            0, 100);

        this.loansButton = new ButtonItem(SheetCategory.ECONOMY, null, save.getGame().getLocalisationClean("AI_LOANS"), 2);
        this.loans = FXCollections.observableArrayList();

        //GOVERNMENT
        this.governmentRankField = new ClearableComboBoxItem<>(SheetCategory.COUNTRY_GOVERNMENT,
                                                               save.getGame().getLocalisation("GOV_RANK"),
                                                               FXCollections.observableArrayList(),
                                                               new ClearableComboBox<>(new RequiredComboBox<>()));
        this.governmentRankField.setConverter(new PairConverter());
        this.governmentRankField.setCellFactory(new PairCellFactory());

        this.governmentReformProgressField = new ClearableSpinnerItem<>(SheetCategory.COUNTRY_GOVERNMENT,
                                                                        save.getGame().getLocalisationCleanNoPunctuation("CHANGE_GOVERNMENT_REFORM_PROGRESS"),
                                                                        new ClearableSpinnerDouble(0, Double.MAX_VALUE, 10));

        this.governmentReformsButton = new ButtonItem(SheetCategory.COUNTRY_GOVERNMENT,
                                                      null,
                                                      save.getGame().getLocalisationClean("governmental_reforms"),
                                                      2);
        this.governmentReformsField = FXCollections.observableArrayList();

        //CATEGORY_COURT
        this.courtPropertySheet = new CustomPropertySheet();
        this.courtPropertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.courtPropertySheet.setMode(CustomPropertySheet.Mode.CATEGORY);
        this.courtPropertySheet.setModeSwitcherVisible(false);
        this.courtPropertySheet.setSearchBoxVisible(false);
        this.courtPropertySheetSkin = new CustomPropertySheetSkin(this.courtPropertySheet);
        this.courtPropertySheet.setSkin(this.courtPropertySheetSkin);

        this.countrySubjectsButton = new ButtonItem(save.getGame().getLocalisationClean("HEADER_SUBJECTS"),
                                                    null,
                                                    save.getGame().getLocalisationClean("HEADER_SUBJECTS"),
                                                    2);

        this.overlordField = new ClearableComboBoxItem<>(save.getGame().getLocalisationClean("HEADER_SUBJECTS"),
                                                         save.getGame().getLocalisation("HEADER_OVERLORD"),
                                                         FXCollections.observableArrayList(new ArrayList<>()),
                                                         new ClearableComboBox<>(new ComboBox<>()));
        this.overlordField.setEditable(false);

        this.manpowerField = new ClearableSpinnerItem<>(SheetCategory.COUNTRY_MILITARY,
                                                        save.getGame().getLocalisationCleanNoPunctuation("HINT_MANPOWER_TITLE"),
                                                        new ClearableSpinnerDouble(0, Double.MAX_VALUE, 1000));

        this.sailorsField = new ClearableSpinnerItem<>(SheetCategory.COUNTRY_MILITARY,
                                                       save.getGame().getLocalisationCleanNoPunctuation("LEDGER_SAILORS"),
                                                       new ClearableSpinnerDouble(0, Double.MAX_VALUE, 1000));

        this.armyTraditionField = new ClearableSliderItem(SheetCategory.COUNTRY_MILITARY,
                                                          save.getGame().getLocalisation("army_tradition"),
                                                          0, 100);

        this.navyTraditionField = new ClearableSliderItem(SheetCategory.COUNTRY_MILITARY,
                                                          save.getGame().getLocalisation("navy_tradition"),
                                                          0, 100);

        this.armyProfessionalismField = new ClearableSliderItem(SheetCategory.COUNTRY_MILITARY,
                                                                save.getGame().getLocalisation("army_professionalism"),
                                                                0, save.getGame().getMaxArmyProfessionalism());

        this.warEhaustionField = new ClearableSliderItem(SheetCategory.COUNTRY_MILITARY,
                                                         save.getGame().getLocalisation("WAR_EXHAUSTION"),
                                                         0, 20);

        //ESTATES
        this.estatesPropertySheet = new CustomPropertySheet();
        this.estatesPropertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.estatesPropertySheet.setMode(CustomPropertySheet.Mode.CATEGORY);
        this.estatesPropertySheet.setModeSwitcherVisible(false);
        this.estatesPropertySheet.setSearchBoxVisible(false);
        this.estatesPropertySheetSkin = new CustomPropertySheetSkin(this.estatesPropertySheet);
        this.estatesPropertySheet.setSkin(this.estatesPropertySheetSkin);

        this.estatePropertySheets = new ArrayList<>();

        //TECHNOLOGY
        this.admTechField = new ClearableSpinnerItem<>(save.getGame().getLocalisationClean("HEADER_TECHNOLOGY"),
                                                       save.getGame().getLocalisationClean("adm_tech"),
                                                       new ClearableSpinnerInt(0, save.getGame().getTechnologies(Power.ADM).size(), 1));

        this.dipTechField = new ClearableSpinnerItem<>(save.getGame().getLocalisationClean("HEADER_TECHNOLOGY"),
                                                       save.getGame().getLocalisationClean("dip_tech"),
                                                       new ClearableSpinnerInt(0, save.getGame().getTechnologies(Power.DIP).size(), 1));

        this.milTechField = new ClearableSpinnerItem<>(save.getGame().getLocalisationClean("HEADER_TECHNOLOGY"),
                                                       save.getGame().getLocalisationClean("mil_tech"),
                                                       new ClearableSpinnerInt(0, save.getGame().getTechnologies(Power.MIL).size(), 1));

        this.innovativenessField = new ClearableSliderItem(save.getGame().getLocalisationClean("HEADER_TECHNOLOGY"),
                                                           save.getGame().getLocalisation("innovativeness"),
                                                           0, save.getGame().getInnovativenessMax());

        this.ideas = FXCollections.observableArrayList();
        this.ideasButton = new ButtonItem(save.getGame().getLocalisationClean("HEADER_TECHNOLOGY"), null,
                                          save.getGame().getLocalisationClean("HEADER_IDEAS"), 2);

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

                String estateExpandedPaneName = this.estatesPropertySheetSkin.getAccordion().getExpandedPane() == null ? null :
                                                this.estatesPropertySheetSkin.getAccordion().getExpandedPane().getText();

                String courtExpandedPaneName = this.courtPropertySheetSkin.getAccordion().getExpandedPane() == null ? null :
                                               this.courtPropertySheetSkin.getAccordion().getExpandedPane().getText();

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

                this.stabilityField.setSupplier(this.country::getStability);
                this.stabilityField.setValue(this.country.getStability());
                items.add(this.stabilityField);

                this.prestigeField.setValue(this.country.getPrestige());
                this.prestigeField.setSupplier(this.country::getPrestige);
                items.add(this.prestigeField);

                this.absolutismField.setValue(this.country.getAbsolutism());
                this.absolutismField.setSupplier(this.country::getAbsolutism);

                if (MapUtils.isNotEmpty(this.country.getSave().getCurrentAge().getAbsolutism())) {
                    items.add(this.absolutismField);
                }

                //Culture
                this.cultureField.setValue(this.country.getPrimaryCulture());
                this.cultureField.setSupplier(this.country::getPrimaryCulture);
                items.add(this.cultureField);

                this.acceptedCulturesField.setValue(FXCollections.observableArrayList(this.country.getAcceptedCultures()));
                this.acceptedCulturesField.setSupplier(this.country::getAcceptedCultures);
                items.add(this.acceptedCulturesField);

                //Economy
                this.treasuryField.setValue(this.country.getTreasury());
                this.treasuryField.setSupplier(this.country::getTreasury);
                items.add(this.treasuryField);

                this.inflationField.setValue(this.country.getInflation());
                this.inflationField.setSupplier(this.country::getInflation);
                items.add(this.inflationField);

                this.corruptionField.setValue(this.country.getCorruption());
                this.corruptionField.setSupplier(this.country::getCorruption);
                items.add(this.corruptionField);

                this.mercantilismField.setValue(this.country.getMercantilism());
                this.mercantilismField.setSupplier(this.country::getMercantilism);
                items.add(this.mercantilismField);

                this.loans.setAll(this.country.getLoans().stream().map(Loan::new).collect(Collectors.toList()));
                this.loansButton.getButton().setOnAction(event -> {
                    TableViewDialog<Loan> dialog = new TableViewDialog<>(this.country.getSave(),
                                                                         new TableView2Loan(this.country, this.loans),
                                                                         this.country.getSave().getGame().getLocalisationClean("AI_LOANS"),
                                                                         (list) -> new Loan(300, 4, this.country.getSave().getDate().plusYears(5)),
                                                                         () -> this.loans);
                    Optional<List<Loan>> countrySubjects = dialog.showAndWait();

                    countrySubjects.ifPresent(this.loans::setAll);
                });
                items.add(this.loansButton);

                //Government
                this.governmentRankField.getChoices().setAll(this.country.getGovernmentName().getRanks().values());
                this.governmentRankField.setValue(this.country.getGovernmentName().getRank(this.country.getGovernmentLevel()));
                this.governmentRankField.setSupplier(() -> this.country.getGovernmentName().getRank(this.country.getGovernmentLevel()));
                items.add(this.governmentRankField);

                this.governmentReformProgressField.setSupplier(this.country::getGovernmentReformProgress);
                this.governmentReformProgressField.setValue(this.country.getGovernmentReformProgress());
                items.add(this.governmentReformProgressField);

                this.governmentReformsField.setAll(this.country.getGovernment().getReforms());
                this.governmentReformsButton.getButton().setOnAction(event -> {
                    GovernmentReformsDialog dialog = new GovernmentReformsDialog(this.country, this.governmentReformsField);

                    Optional<List<GovernmentReform>> reforms = dialog.showAndWait();

                    reforms.ifPresent(this.governmentReformsField::setAll);
                });
                items.add(this.governmentReformsButton);

                //Court
                this.courtPropertySheet.getItems().clear();
                this.monarchPropertySheet = null;

                if (this.country.getMonarch() != null) {
                    MonarchPropertySheet sheet = new MonarchPropertySheet(this.country, this.country.getMonarch(),
                                                                          this.country.getSave().getGame().getLocalisationClean("CURRENT_MONARCH"),
                                                                          this.cultures, this.religions);

                    if (!sheet.getPropertySheet().getItems().isEmpty()) {
                        this.monarchPropertySheet = sheet;
                        this.courtPropertySheet.getItems().addAll(sheet.getPropertySheet().getItems());
                    }
                }

                this.heirPropertySheet = null;

                if (this.country.getHeir() != null) {
                    MonarchPropertySheet sheet = new MonarchPropertySheet(this.country, this.country.getHeir(),
                                                                          this.country.getSave().getGame().getLocalisationClean("HEIR"),
                                                                          this.cultures, this.religions);

                    if (!sheet.getPropertySheet().getItems().isEmpty()) {
                        this.heirPropertySheet = sheet;
                        this.courtPropertySheet.getItems().addAll(sheet.getPropertySheet().getItems());
                    }
                }

                this.queenPropertySheet = null;

                if (this.country.getQueen() != null) {
                    MonarchPropertySheet sheet = new MonarchPropertySheet(this.country, this.country.getQueen(),
                                                                          this.country.getSave().getGame().getLocalisationClean("CONSORT"),
                                                                          this.cultures, this.religions);

                    if (!sheet.getPropertySheet().getItems().isEmpty()) {
                        this.queenPropertySheet = sheet;
                        this.courtPropertySheet.getItems().addAll(sheet.getPropertySheet().getItems());
                    }
                }

                if (!this.courtPropertySheet.getItems().isEmpty()) {
                    items.add(new PropertySheetItem(this.country.getSave().getGame().getLocalisation("CATEGORY_COURT"), this.courtPropertySheet));
                }

                //Subjects
                this.overlordField.getChoices().setAll(this.country.getOverlord());
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
                                                  (list) -> new CountrySubject(this.country,
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

                //Military
                this.manpowerField.setSupplier(() -> this.country.getManpower() * 1000);
                this.manpowerField.setValue(this.country.getManpower() * 1000);
                items.add(this.manpowerField);

                this.sailorsField.setSupplier(this.country::getSailors);
                this.sailorsField.setValue(this.country.getSailors());
                items.add(this.sailorsField);

                this.armyTraditionField.setValue(this.country.getArmyTradition());
                this.armyTraditionField.setSupplier(this.country::getArmyTradition);
                items.add(this.armyTraditionField);

                this.navyTraditionField.setValue(this.country.getNavyTradition());
                this.navyTraditionField.setSupplier(this.country::getNavyTradition);
                items.add(this.navyTraditionField);

                this.armyProfessionalismField.setValue(this.country.getArmyProfessionalism());
                this.armyProfessionalismField.setSupplier(this.country::getArmyProfessionalism);
                items.add(this.armyProfessionalismField);

                this.warEhaustionField.setValue(this.country.getWarExhaustion());
                this.warEhaustionField.setSupplier(this.country::getWarExhaustion);
                items.add(this.warEhaustionField);

                //Estates
                this.estatePropertySheets.clear();
                this.estatesPropertySheet.getItems().clear();
                this.country.getEstates()
                            .stream()
                            .sorted(Comparator.comparing(estate -> estate.getEstateGame().getLocalizedName(), Eu4Utils.COLLATOR))
                            .forEach(estate -> {
                                EstatePropertySheet estatePropertySheet = new EstatePropertySheet(this.country, estate);

                                if (!estatePropertySheet.getPropertySheet().getItems().isEmpty()) {
                                    this.estatePropertySheets.add(estatePropertySheet);
                                    this.estatesPropertySheet.getItems().addAll(estatePropertySheet.getPropertySheet().getItems());
                                }
                            });
                this.estatePropertySheets.forEach(sheet -> this.estatePropertySheets.stream()
                                                                                    .filter(otherSheet -> !otherSheet.equals(sheet))
                                                                                    .forEach(otherSheet -> sheet.getCountryEstatesTerritory()
                                                                                                                .add(otherSheet.territoryValue())));

                if (!this.estatesPropertySheet.getItems().isEmpty()) {
                    items.add(new PropertySheetItem(this.country.getSave().getGame().getLocalisation("HEADER_ESTATES"), this.estatesPropertySheet));
                }

                //Technology
                this.admTechField.setSupplier(() -> this.country.getTech().getAdm());
                this.admTechField.setValue(this.country.getTech().getAdm());
                items.add(this.admTechField);

                this.dipTechField.setSupplier(() -> this.country.getTech().getDip());
                this.dipTechField.setValue(this.country.getTech().getDip());
                items.add(this.dipTechField);

                this.milTechField.setSupplier(() -> this.country.getTech().getMil());
                this.milTechField.setValue(this.country.getTech().getMil());
                items.add(this.milTechField);

                this.innovativenessField.setValue(this.country.getInnovativeness());
                this.innovativenessField.setSupplier(this.country::getInnovativeness);
                items.add(this.innovativenessField);

                this.ideas.setAll(this.country.getIdeaGroups().getIdeaGroups().entrySet().stream().map(Idea::new).collect(Collectors.toList()));
                this.ideasButton.getButton().setOnAction(event -> {
                    TableView2Ideas tableView2Ideas = new TableView2Ideas(this.country, this.ideas, this.country.getSave()
                                                                                                                .getGame()
                                                                                                                .getIdeaGroups()
                                                                                                                .stream()
                                                                                                                .collect(Collectors.toCollection(
                                                                                                                        FXCollections::observableArrayList)));
                    TableViewDialog<Idea> dialog =
                            new TableViewDialog<>(this.country.getSave(),
                                                  tableView2Ideas,
                                                  this.country.getSave().getGame().getLocalisationClean("HEADER_IDEAS"),
                                                  (list) -> new Idea(this.country.getSave()
                                                                                 .getGame()
                                                                                 .getIdeaGroups()
                                                                                 .stream()
                                                                                 .filter(ideaGroup -> list.stream()
                                                                                                          .noneMatch(i -> i.getIdeaGroup().equals(ideaGroup)))
                                                                                 .filter(ideaGroup -> list.isEmpty() == ideaGroup.isFree())
                                                                                 .findFirst()
                                                                                 .get(),
                                                                     0),
                                                  () -> this.ideas);
                    dialog.setDisableAddProperty(tableView2Ideas.disableAddPropertyProperty());
                    Optional<List<Idea>> ideas = dialog.showAndWait();

                    ideas.ifPresent(this.ideas::setAll);
                });
                items.add(this.ideasButton);

                this.propertySheet.getItems().setAll(items);

                if (expandedPaneName != null) {
                    this.propertySheetSkin.getAccordion()
                                          .getPanes()
                                          .stream()
                                          .filter(titledPane -> titledPane.getText().equals(expandedPaneName))
                                          .findFirst()
                                          .ifPresent(titledPane -> this.propertySheetSkin.getAccordion().setExpandedPane(titledPane));
                }

                if (estateExpandedPaneName != null) {
                    this.estatesPropertySheetSkin.getAccordion()
                                                 .getPanes()
                                                 .stream()
                                                 .filter(titledPane -> titledPane.getText().equals(estateExpandedPaneName))
                                                 .findFirst()
                                                 .ifPresent(titledPane -> this.estatesPropertySheetSkin.getAccordion().setExpandedPane(titledPane));
                }

                if (courtExpandedPaneName != null) {
                    this.courtPropertySheetSkin.getAccordion()
                                               .getPanes()
                                               .stream()
                                               .filter(titledPane -> titledPane.getText().equals(courtExpandedPaneName))
                                               .findFirst()
                                               .ifPresent(titledPane -> this.courtPropertySheetSkin.getAccordion().setExpandedPane(titledPane));
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

        if (!Objects.equals(this.country.getTreasury(), this.treasuryField.getTrueValue())) {
            this.country.setTreasury(this.treasuryField.getTrueValue());
        }

        if (!Objects.equals(this.country.getCorruption(), this.corruptionField.getDoubleValue())) {
            this.country.setCorruption(this.corruptionField.getDoubleValue());
        }

        if (!Objects.equals(this.country.getMercantilism(), this.mercantilismField.getIntValue())) {
            this.country.setMercantilism(this.mercantilismField.getIntValue());
        }

        if (this.country.getLoans().size() != this.loans.size() || this.loans.stream().anyMatch(Loan::isChanged)) {
            this.country.getLoans().forEach(loan -> this.loans.stream()
                                                              .filter(l -> loan.getId().getId().equals(l.getId()))
                                                              .findFirst()
                                                              .ifPresentOrElse(l -> {
                                                                                   if (l.getAmount() != loan.getAmount()) {
                                                                                       loan.setAmount(l.getAmount());
                                                                                   }

                                                                                   if (l.getInterest() != loan.getInterest()) {
                                                                                       loan.setInterest(l.getInterest());
                                                                                   }

                                                                                   if (!Objects.equals(l.getExpiryDate(), loan.getExpiryDate())) {
                                                                                       loan.setExpiryDate(l.getExpiryDate());
                                                                                   }
                                                                                   this.loans.remove(l);
                                                                               },
                                                                               () -> this.country.removeLoan(loan.getId().getId())));
            this.loans.forEach(l -> this.country.addLoan(l.getInterest(), l.getAmount(), l.getExpiryDate()));
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

        if (!Objects.equals(this.country.getStability(), this.stabilityField.getTrueValue())) {
            this.country.setStability(this.stabilityField.getTrueValue());
        }

        if (!Objects.equals(this.country.getPrestige(), this.prestigeField.getTrueValue())) {
            this.country.setPrestige(this.prestigeField.getTrueValue());
        }

        if (MapUtils.isNotEmpty(this.country.getSave().getCurrentAge().getAbsolutism())) {
            if (!Objects.equals(this.country.getAbsolutism(), this.absolutismField.getTrueValue())) {
                this.country.setAbsolutism(this.absolutismField.getTrueValue());
            }
        }

        if (!Objects.deepEquals(this.country.getPrimaryCulture(), this.cultureField.getSelectedValue())) {
            this.country.setPrimaryCulture(this.cultureField.getSelectedValue());
        }

        if (!Objects.deepEquals(this.country.getAcceptedCultures(), this.acceptedCulturesField.getSelectedValues())) {
            this.country.setAcceptedCulture(new ArrayList<>(this.acceptedCulturesField.getSelectedValues()));
        }

        if (!Objects.equals(this.country.getGovernmentReformProgress(), this.governmentReformProgressField.getTrueValue())) {
            this.country.setGovernmentReformProgress(this.governmentReformProgressField.getTrueValue());
        }

        if (!Objects.equals(this.country.getManpower() * 1000, this.manpowerField.getTrueValue())) {
            this.country.setManpower(this.manpowerField.getTrueValue() / 1000);
        }

        if (!Objects.equals(this.country.getSailors(), this.sailorsField.getTrueValue())) {
            this.country.setSailors(this.sailorsField.getTrueValue());
        }

        if (!Objects.equals(this.country.getArmyTradition(), this.armyTraditionField.getDoubleValue())) {
            this.country.setArmyTradition(this.armyTraditionField.getDoubleValue());
        }

        if (!Objects.equals(this.country.getNavyTradition(), this.navyTraditionField.getDoubleValue())) {
            this.country.setNavyTradition(this.navyTraditionField.getDoubleValue());
        }

        if (!Objects.equals(this.country.getArmyProfessionalism(), this.armyProfessionalismField.getDoubleValue())) {
            this.country.setArmyProfessionalism(this.armyProfessionalismField.getDoubleValue());
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

        this.estatePropertySheets.forEach(sheet -> sheet.validate(actionEvent));

        if (!Objects.equals(this.country.getTech().getAdm(), this.admTechField.getTrueValue())) {
            this.country.getTech().setAdm(this.admTechField.getTrueValue());
        }

        if (!Objects.equals(this.country.getTech().getDip(), this.dipTechField.getTrueValue())) {
            this.country.getTech().setDip(this.dipTechField.getTrueValue());
        }

        if (!Objects.equals(this.country.getTech().getMil(), this.milTechField.getTrueValue())) {
            this.country.getTech().setMil(this.milTechField.getTrueValue());
        }

        if (!Objects.equals(this.country.getInnovativeness(), this.innovativenessField.getDoubleValue())) {
            this.country.setInnovativeness(this.innovativenessField.getDoubleValue());
        }

        if (this.country.getIdeaGroups().getIdeaGroups().size() != this.ideas.size() || this.ideas.stream().anyMatch(Idea::isChanged)) {
            this.country.getIdeaGroups()
                        .getIdeaGroups()
                        .forEach((ideaGroup, integer) -> this.ideas.stream()
                                                                   .filter(p -> ideaGroup.equals(p.getIdeaGroup()))
                                                                   .findFirst()
                                                                   .ifPresentOrElse(p -> {
                                                                                        if (!Objects.equals(p.getLevel(), integer)) {
                                                                                            this.country.getIdeaGroups().setIdeaGroup(ideaGroup, p.getLevel());
                                                                                        }

                                                                                        this.ideas.remove(p);
                                                                                    },
                                                                                    () -> this.country.getIdeaGroups().removeIdeaGroup(ideaGroup)));
            this.ideas.forEach(idea -> this.country.getIdeaGroups().setIdeaGroup(idea.getIdeaGroup(), idea.getLevel()));
        }

        if (this.monarchPropertySheet != null) {
            this.monarchPropertySheet.validate(actionEvent);
        }

        if (this.heirPropertySheet != null) {
            this.heirPropertySheet.validate(actionEvent);
        }

        if (this.queenPropertySheet != null) {
            this.queenPropertySheet.validate(actionEvent);
        }

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
