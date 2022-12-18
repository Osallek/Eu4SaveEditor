package fr.osallek.eu4saveeditor.controller.mapview;

import fr.osallek.clausewitzparser.common.ClausewitzUtils;
import fr.osallek.eu4parser.common.Eu4Utils;
import fr.osallek.eu4parser.model.Power;
import fr.osallek.eu4parser.model.game.Culture;
import fr.osallek.eu4parser.model.game.FetishistCult;
import fr.osallek.eu4parser.model.game.GovernmentReform;
import fr.osallek.eu4parser.model.game.PersonalDeity;
import fr.osallek.eu4parser.model.game.Policy;
import fr.osallek.eu4parser.model.game.Religion;
import fr.osallek.eu4parser.model.game.ReligionGroup;
import fr.osallek.eu4parser.model.game.ReligiousReform;
import fr.osallek.eu4parser.model.game.SubjectType;
import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.SaveReligion;
import fr.osallek.eu4parser.model.save.country.LeaderType;
import fr.osallek.eu4parser.model.save.country.SaveCountry;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import fr.osallek.eu4saveeditor.Eu4SaveEditor;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import fr.osallek.eu4saveeditor.controller.control.ClearableCheckComboBox;
import fr.osallek.eu4saveeditor.controller.control.ClearableColorPicker;
import fr.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import fr.osallek.eu4saveeditor.controller.control.ClearableSpinnerDouble;
import fr.osallek.eu4saveeditor.controller.control.ClearableSpinnerInt;
import fr.osallek.eu4saveeditor.controller.control.CustomListSelectionView;
import fr.osallek.eu4saveeditor.controller.control.RequiredComboBox;
import fr.osallek.eu4saveeditor.controller.control.TableView2CountrySubject;
import fr.osallek.eu4saveeditor.controller.control.TableView2Ideas;
import fr.osallek.eu4saveeditor.controller.control.TableView2Leader;
import fr.osallek.eu4saveeditor.controller.control.TableView2Loan;
import fr.osallek.eu4saveeditor.controller.control.TableView2Modifier;
import fr.osallek.eu4saveeditor.controller.control.TableView2Policy;
import fr.osallek.eu4saveeditor.controller.control.TableView2Rival;
import fr.osallek.eu4saveeditor.controller.control.TableView2StringDate;
import fr.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.CultureStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.CultureStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.FetishistCultStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.FetishistCultStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.PairCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.PairConverter;
import fr.osallek.eu4saveeditor.controller.converter.PersonalDeityStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.PersonalDeityStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.ProvinceStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.ProvinceStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.ReligionGroupStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.ReligionStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.ReligionStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.ReligiousReformStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.SaveReligionStringCellFactory;
import fr.osallek.eu4saveeditor.controller.object.ActivePolicy;
import fr.osallek.eu4saveeditor.controller.object.CountrySubject;
import fr.osallek.eu4saveeditor.controller.object.Idea;
import fr.osallek.eu4saveeditor.controller.object.Leader;
import fr.osallek.eu4saveeditor.controller.object.Loan;
import fr.osallek.eu4saveeditor.controller.object.Modifier;
import fr.osallek.eu4saveeditor.controller.object.Rival;
import fr.osallek.eu4saveeditor.controller.object.StringDate;
import fr.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import fr.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import fr.osallek.eu4saveeditor.controller.pane.GovernmentReformsDialog;
import fr.osallek.eu4saveeditor.controller.pane.ListSelectionViewDialog;
import fr.osallek.eu4saveeditor.controller.pane.TableViewDialog;
import fr.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ButtonItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.CheckBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableCheckComboBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableColorPickerItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderIntItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSpinnerItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.PropertySheetItem;
import fr.osallek.eu4saveeditor.controller.validator.CustomGraphicValidationDecoration;
import fr.osallek.eu4saveeditor.i18n.SheetCategory;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.BooleanPropertyBase;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.ColorPicker;
import javafx.scene.control.ComboBox;
import javafx.scene.layout.VBox;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.controlsfx.control.SearchableComboBox;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.Validator;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CountryPropertySheet extends VBox {

    private static final Logger LOGGER = LoggerFactory.getLogger(CountryPropertySheet.class);

    private SaveCountry country;

    private final ObservableList<Culture> cultures;

    private final ObservableList<SaveReligion> religions;

    private final ObservableList<SaveCountry> countriesAlive;

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

    private final ClearableComboBoxItem<SaveProvince> capitalField;

    private final ClearableColorPickerItem mapColorField;

    private final ClearableComboBoxItem<Culture> cultureField;

    private final ClearableCheckComboBoxItem<Culture> acceptedCulturesField;

    private final ClearableComboBoxItem<SaveReligion> religionField;

    private final ClearableSliderItem authorityField;

    private final ClearableSliderItem churchPowerField;

    private final ClearableSliderItem fervorField;

    private final ButtonItem religiousReformsButton;

    private ObservableList<ReligiousReform> passedReligiousReforms;

    private ObservableList<ReligiousReform> notPassedReligiousReforms;

    private final ClearableSliderItem patriarchAuthorityField;

    private final ClearableComboBoxItem<FetishistCult> fetishistCultField;

    private final ClearableSpinnerItem<Integer> isolationismField;

    private final ClearableSliderIntItem karmaField;

    private final ClearableSliderItem pietyField;

    private final ClearableSliderItem harmonyField;

    private final ClearableComboBoxItem<PersonalDeity> personalDeityField;

    private final ButtonItem harmonizedReligionGroupsButton;

    private ObservableList<ReligionGroup> harmonizedReligionGroups;

    private ObservableList<ReligionGroup> notHarmonizedReligionGroups;

    private final ButtonItem harmonizedReligionsButton;

    private ObservableList<Religion> harmonizedReligions;

    private ObservableList<Religion> notHarmonizedReligions;

    private final ClearableSliderItem doomField;

    private final ClearableComboBoxItem<SaveReligion> secondaryReligionsField;

    private final ClearableSpinnerItem<Double> governmentReformProgressField;

    private final ClearableComboBoxItem<Pair<Integer, String>> governmentRankField;

    private final ButtonItem governmentReformsButton;

    private final ObservableList<GovernmentReform> governmentReformsField;

    private final CustomPropertySheet courtPropertySheet;

    private CustomPropertySheetSkin courtPropertySheetSkin;

    private MonarchPropertySheet monarchPropertySheet;

    private MonarchPropertySheet heirPropertySheet;

    private MonarchPropertySheet queenPropertySheet;

    private final ButtonItem countrySubjectsButton;

    private Map<SaveCountry, CountrySubject> countrySubjectsField;

    private final ClearableComboBoxItem<SaveCountry> overlordField;

    private final ButtonItem rivalsButton;

    private final ObservableList<Rival> rivals;

    private final ClearableSpinnerItem<Double> treasuryField;

    private final ClearableSpinnerItem<Double> inflationField;

    private final ClearableSliderItem corruptionField;

    private final ClearableSliderIntItem mercantilismField;

    private final List<CheckBoxItem> institutionsEmbracedFields;

    private final ClearableSpinnerItem<Double> manpowerField;

    private final ClearableSpinnerItem<Double> sailorsField;

    private final ClearableSliderItem armyTraditionField;

    private final ClearableSliderItem navyTraditionField;

    private final ClearableSliderItem armyProfessionalismField;

    private final ClearableSliderItem warEhaustionField;

    private final ButtonItem leadersButton;

    private final ObservableList<Leader> leaders;

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

    private final ButtonItem admPoliciesButton;

    private ObservableList<Policy> availableAdmPolicies;

    private final ObservableList<ActivePolicy> admPolicies;

    private final ButtonItem dipPoliciesButton;

    private ObservableList<Policy> availableDipPolicies;

    private final ObservableList<ActivePolicy> dipPolicies;

    private final ButtonItem milPoliciesButton;

    private ObservableList<Policy> availableMilPolicies;

    private final ObservableList<ActivePolicy> milPolicies;

    private final ButtonItem modifiersButton;

    private final ObservableList<Modifier> modifiers;

    private final ObservableList<StringDate> flags;

    private final ButtonItem flagsButton;

    private final ObservableList<StringDate> hiddenFlags;

    private final ButtonItem hiddenFlagsButton;

    private final CustomPropertySheetSkin propertySheetSkin;

    private BooleanProperty colorChanged;

    public CountryPropertySheet(Save save, ObservableList<SaveCountry> countriesAlive, ObservableList<Culture> cultures, ObservableList<SaveReligion> religions) {
        this.countriesAlive = countriesAlive;
        this.cultures = cultures;
        this.religions = religions;
        this.subjectTypes = save.getGame()
                                .getSubjectTypes()
                                .stream()
                                .filter(type -> !"default".equals(type.getName()))
                                .sorted(Comparator.comparing(t -> Eu4SaveEditorUtils.localize(t.getName(), save.getGame()), Eu4Utils.COLLATOR))
                                .collect(Collectors.toCollection(FXCollections::observableArrayList));
        this.propertySheet = new CustomPropertySheet();
        this.propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.propertySheet.setMode(CustomPropertySheet.Mode.CATEGORY);
        this.propertySheet.setModeSwitcherVisible(false);
        this.propertySheet.setSearchBoxVisible(false);

        this.propertySheetSkin = new CustomPropertySheetSkin(this.propertySheet);
        this.propertySheet.setSkin(this.propertySheetSkin);

        //GENERAL
        this.nameField = new ClearableTextItem(SheetCategory.GENERAL, Eu4SaveEditorUtils.localize("LEDGER_NAME", save.getGame()));
        this.nameField.getTextField().getStylesheets().add(Eu4SaveEditor.class.getResource("/styles/style.css").toExternalForm());

        this.wasPlayerField = new CheckBoxItem(SheetCategory.GENERAL, Eu4SaveEditorUtils.localize("WAS_PLAYER", save.getGame()), false);

        this.admPointField = new ClearableSpinnerItem<>(SheetCategory.GENERAL,
                                                        Eu4SaveEditorUtils.localize("ADM_POWER", save.getGame()),
                                                        new ClearableSpinnerInt(0, 999, 1));

        this.dipPointField = new ClearableSpinnerItem<>(SheetCategory.GENERAL,
                                                        Eu4SaveEditorUtils.localize("DIP_POWER", save.getGame()),
                                                        new ClearableSpinnerInt(0, 999, 1));

        this.milPointField = new ClearableSpinnerItem<>(SheetCategory.GENERAL,
                                                        Eu4SaveEditorUtils.localize("MIL_POWER", save.getGame()),
                                                        new ClearableSpinnerInt(0, 999, 1));

        this.stabilityField = new ClearableSpinnerItem<>(SheetCategory.GENERAL,
                                                         Eu4SaveEditorUtils.localize("stability", save.getGame()),
                                                         new ClearableSpinnerInt(-3, 3, 1));

        this.prestigeField = new ClearableSpinnerItem<>(SheetCategory.GENERAL,
                                                        Eu4SaveEditorUtils.localize("prestige", save.getGame()),
                                                        new ClearableSpinnerDouble(-100, 100, 1));

        this.absolutismField = new ClearableSpinnerItem<>(SheetCategory.GENERAL,
                                                          Eu4SaveEditorUtils.localize("absolutism", save.getGame()),
                                                          new ClearableSpinnerInt(0, 100, 1));

        this.capitalField = new ClearableComboBoxItem<>(SheetCategory.GENERAL,
                                                        Eu4SaveEditorUtils.localize("TRIGGER_CAPITAL", save.getGame()),
                                                        FXCollections.observableArrayList(),
                                                        new ClearableComboBox<>(new SearchableComboBox<>()));
        this.capitalField.setConverter(new ProvinceStringConverter());
        this.capitalField.setCellFactory(new ProvinceStringCellFactory());

        this.mapColorField = new ClearableColorPickerItem(SheetCategory.GENERAL,
                                                          Eu4SaveEditorUtils.localize("ND_MAP_COLOR", save.getGame()),
                                                          new ClearableColorPicker(new ColorPicker()));

        //Culture
        this.cultureField = new ClearableComboBoxItem<>(Eu4SaveEditorUtils.localize("LEDGER_CULTURE", save.getGame()),
                                                        Eu4SaveEditorUtils.localize("LEDGER_CULTURE", save.getGame()),
                                                        cultures,
                                                        new ClearableComboBox<>(new SearchableComboBox<>()));
        this.cultureField.setConverter(new CultureStringConverter(save.getGame()));
        this.cultureField.setCellFactory(new CultureStringCellFactory(save.getGame()));

        this.acceptedCulturesField = new ClearableCheckComboBoxItem<>(Eu4SaveEditorUtils.localize("LEDGER_CULTURE", save.getGame()),
                                                                      Eu4SaveEditorUtils.localize("MAPMODE_ACCEPTEDCULTURES", save.getGame()),
                                                                      cultures,
                                                                      new ClearableCheckComboBox<>());
        this.acceptedCulturesField.setConverter(new CultureStringConverter(save.getGame()));

        //Religion
        this.religionField = new ClearableComboBoxItem<>(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                         Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                         this.religions,
                                                         new ClearableComboBox<>(new SearchableComboBox<>()));
        this.religionField.setConverter(new ReligionStringConverter(save.getGame()));
        this.religionField.setCellFactory(new SaveReligionStringCellFactory(save.getGame()));

        this.authorityField = new ClearableSliderItem(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                      Eu4SaveEditorUtils.localize("authority", save.getGame()),
                                                      0, 100);

        this.churchPowerField = new ClearableSliderItem(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                        Eu4SaveEditorUtils.localize("MODIFIER_CHURCH_POWER", save.getGame()),
                                                        0, 200);

        this.fervorField = new ClearableSliderItem(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                   Eu4SaveEditorUtils.localize("FERVOR_VALUE2", save.getGame()),
                                                   0, 100);

        this.religiousReformsButton = new ButtonItem(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                     null,
                                                     Eu4SaveEditorUtils.localize("HEADER_RELIGIOUS_REFORMS", save.getGame()),
                                                     2);
        this.passedReligiousReforms = FXCollections.observableArrayList();
        this.notPassedReligiousReforms = FXCollections.observableArrayList();

        this.patriarchAuthorityField = new ClearableSliderItem(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                               Eu4SaveEditorUtils.localize("patriarch_authority_global", save.getGame()),
                                                               0, 100);

        this.fetishistCultField = new ClearableComboBoxItem<>(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                              Eu4SaveEditorUtils.localize("HAS_ADOPTED_CULT", save.getGame()),
                                                              FXCollections.observableArrayList(),
                                                              new ClearableComboBox<>(new SearchableComboBox<>()));
        this.fetishistCultField.setConverter(new FetishistCultStringConverter(save.getGame()));
        this.fetishistCultField.setCellFactory(new FetishistCultStringCellFactory(save.getGame()));

        this.isolationismField = new ClearableSpinnerItem<>(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                            Eu4SaveEditorUtils.localize("ISOLATIONISM", save.getGame()),
                                                            new ClearableSpinnerInt(0, 4, 1));

        this.karmaField = new ClearableSliderIntItem(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                     Eu4SaveEditorUtils.localize("CURRENT_KARMA", save.getGame()),
                                                     -100, 100);

        this.pietyField = new ClearableSliderItem(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                  Eu4SaveEditorUtils.localize("CURRENT_PIETY", save.getGame()),
                                                  0, 100);

        this.harmonyField = new ClearableSliderItem(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                    Eu4SaveEditorUtils.localize("CURRENT_HARMONY", save.getGame()),
                                                    0, 100);

        this.harmonizedReligionGroupsButton = new ButtonItem(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                             null,
                                                             Eu4SaveEditorUtils.localize("HARMONIZED_RELIGION_GROUP", save.getGame()),
                                                             2);
        this.harmonizedReligionGroups = FXCollections.observableArrayList();
        this.notHarmonizedReligionGroups = FXCollections.observableArrayList();

        this.harmonizedReligionsButton = new ButtonItem(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                        null,
                                                        Eu4SaveEditorUtils.localize("HARMONIZED_RELIGION", save.getGame()),
                                                        2);
        this.harmonizedReligions = FXCollections.observableArrayList();
        this.notHarmonizedReligions = FXCollections.observableArrayList();

        this.personalDeityField = new ClearableComboBoxItem<>(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                              Eu4SaveEditorUtils.localize("HAS_DEITY", save.getGame()),
                                                              FXCollections.observableArrayList(),
                                                              new ClearableComboBox<>(new SearchableComboBox<>()));
        this.personalDeityField.setConverter(new PersonalDeityStringConverter(save.getGame()));
        this.personalDeityField.setCellFactory(new PersonalDeityStringCellFactory(save.getGame()));

        this.doomField = new ClearableSliderItem(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                 Eu4SaveEditorUtils.localize("doom", save.getGame()),
                                                 0, 100);

        this.secondaryReligionsField = new ClearableComboBoxItem<>(Eu4SaveEditorUtils.localize("LEDGER_RELIGION", save.getGame()),
                                                                   Eu4SaveEditorUtils.localize("SECONDARY_RELIGION", save.getGame()),
                                                                   FXCollections.observableArrayList(this.religions),
                                                                   new ClearableComboBox<>(new SearchableComboBox<>()));
        this.secondaryReligionsField.setConverter(new ReligionStringConverter(save.getGame()));
        this.secondaryReligionsField.setCellFactory(new SaveReligionStringCellFactory(save.getGame()));

        //Economy
        this.treasuryField = new ClearableSpinnerItem<>(SheetCategory.ECONOMY,
                                                        Eu4SaveEditorUtils.localize("TECH_TRESURY_TITLE", save.getGame()),
                                                        new ClearableSpinnerDouble(-1000000, 1000000, 100));

        this.inflationField = new ClearableSpinnerItem<>(SheetCategory.ECONOMY,
                                                         Eu4SaveEditorUtils.localize("inflation", save.getGame()),
                                                         new ClearableSpinnerDouble(0, Integer.MAX_VALUE / 1000.0, 1));

        this.corruptionField = new ClearableSliderItem(SheetCategory.ECONOMY,
                                                       Eu4SaveEditorUtils.localize("corruption", save.getGame()),
                                                       0, 100);

        this.mercantilismField = new ClearableSliderIntItem(SheetCategory.ECONOMY,
                                                            Eu4SaveEditorUtils.localize("MERCANTILISM_LABEL", save.getGame()),
                                                            0, 100);

        this.loansButton = new ButtonItem(SheetCategory.ECONOMY, null, save.getGame().getLocalisationClean("AI_LOANS", Eu4Language.getDefault()), 2);
        this.loans = FXCollections.observableArrayList();

        //Institutions
        this.institutionsEmbracedFields = new ArrayList<>();

        //GOVERNMENT
        this.governmentRankField = new ClearableComboBoxItem<>(SheetCategory.COUNTRY_GOVERNMENT,
                                                               save.getGame().getLocalisationClean("GOV_RANK", Eu4Language.getDefault()),
                                                               FXCollections.observableArrayList(),
                                                               new ClearableComboBox<>(new RequiredComboBox<>()));
        this.governmentRankField.setConverter(new PairConverter());
        this.governmentRankField.setCellFactory(new PairCellFactory());

        this.governmentReformProgressField = new ClearableSpinnerItem<>(SheetCategory.COUNTRY_GOVERNMENT,
                                                                        save.getGame()
                                                                            .getLocalisationCleanNoPunctuation("CHANGE_GOVERNMENT_REFORM_PROGRESS", Eu4Language.getDefault()),
                                                                        new ClearableSpinnerDouble(0, Double.MAX_VALUE, 10));

        this.governmentReformsButton = new ButtonItem(SheetCategory.COUNTRY_GOVERNMENT,
                                                      null,
                                                      save.getGame().getLocalisationClean("governmental_reforms", Eu4Language.getDefault()),
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

        //Diplomacy
        this.countrySubjectsButton = new ButtonItem(save.getGame().getLocalisationClean("HEADER_DIPLOMACY", Eu4Language.getDefault()),
                                                    null,
                                                    save.getGame().getLocalisationClean("HEADER_SUBJECTS", Eu4Language.getDefault()),
                                                    2);

        this.overlordField = new ClearableComboBoxItem<>(save.getGame().getLocalisationClean("HEADER_DIPLOMACY", Eu4Language.getDefault()),
                                                         save.getGame().getLocalisationClean("HEADER_OVERLORD", Eu4Language.getDefault()),
                                                         FXCollections.observableArrayList(new ArrayList<>()),
                                                         new ClearableComboBox<>(new ComboBox<>()));
        this.overlordField.setEditable(false);

        this.rivalsButton = new ButtonItem(save.getGame().getLocalisationClean("HEADER_DIPLOMACY", Eu4Language.getDefault()), null,
                                           save.getGame().getLocalisationClean("RIVALS", Eu4Language.getDefault()), 2);
        this.rivals = FXCollections.observableArrayList();

        //Military
        this.manpowerField = new ClearableSpinnerItem<>(SheetCategory.COUNTRY_MILITARY,
                                                        save.getGame().getLocalisationCleanNoPunctuation("HINT_MANPOWER_TITLE", Eu4Language.getDefault()),
                                                        new ClearableSpinnerDouble(0, Double.MAX_VALUE, 1000));

        this.sailorsField = new ClearableSpinnerItem<>(SheetCategory.COUNTRY_MILITARY,
                                                       save.getGame().getLocalisationCleanNoPunctuation("LEDGER_SAILORS", Eu4Language.getDefault()),
                                                       new ClearableSpinnerDouble(0, Double.MAX_VALUE, 1000));

        this.armyTraditionField = new ClearableSliderItem(SheetCategory.COUNTRY_MILITARY,
                                                          Eu4SaveEditorUtils.localize("army_tradition", save.getGame()),
                                                          0, 100);

        this.navyTraditionField = new ClearableSliderItem(SheetCategory.COUNTRY_MILITARY,
                                                          Eu4SaveEditorUtils.localize("navy_tradition", save.getGame()),
                                                          0, 100);

        this.armyProfessionalismField = new ClearableSliderItem(SheetCategory.COUNTRY_MILITARY,
                                                                Eu4SaveEditorUtils.localize("army_professionalism", save.getGame()),
                                                                0, save.getGame().getMaxArmyProfessionalism());

        this.warEhaustionField = new ClearableSliderItem(SheetCategory.COUNTRY_MILITARY,
                                                         Eu4SaveEditorUtils.localize("WAR_EXHAUSTION", save.getGame()),
                                                         0, 20);

        this.leadersButton = new ButtonItem(SheetCategory.COUNTRY_MILITARY, null, save.getGame()
                                                                                      .getLocalisationClean("HEADER_LEADER", Eu4Language.getDefault()), 2);
        this.leaders = FXCollections.observableArrayList();

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
        this.admTechField = new ClearableSpinnerItem<>(save.getGame().getLocalisationClean("HEADER_TECHNOLOGY", Eu4Language.getDefault()),
                                                       save.getGame().getLocalisationClean("adm_tech", Eu4Language.getDefault()),
                                                       new ClearableSpinnerInt(0, save.getGame().getTechnologies(Power.ADM).size(), 1));

        this.dipTechField = new ClearableSpinnerItem<>(save.getGame().getLocalisationClean("HEADER_TECHNOLOGY", Eu4Language.getDefault()),
                                                       save.getGame().getLocalisationClean("dip_tech", Eu4Language.getDefault()),
                                                       new ClearableSpinnerInt(0, save.getGame().getTechnologies(Power.DIP).size(), 1));

        this.milTechField = new ClearableSpinnerItem<>(save.getGame().getLocalisationClean("HEADER_TECHNOLOGY", Eu4Language.getDefault()),
                                                       save.getGame().getLocalisationClean("mil_tech", Eu4Language.getDefault()),
                                                       new ClearableSpinnerInt(0, save.getGame().getTechnologies(Power.MIL).size(), 1));

        this.innovativenessField = new ClearableSliderItem(save.getGame().getLocalisationClean("HEADER_TECHNOLOGY", Eu4Language.getDefault()),
                                                           save.getGame().getLocalisationClean("innovativeness", Eu4Language.getDefault()),
                                                           0, save.getGame().getInnovativenessMax());

        this.ideas = FXCollections.observableArrayList();
        this.ideasButton = new ButtonItem(save.getGame().getLocalisationClean("HEADER_TECHNOLOGY", Eu4Language.getDefault()), null,
                                          save.getGame().getLocalisationClean("HEADER_IDEAS", Eu4Language.getDefault()), 2);

        this.admPolicies = FXCollections.observableArrayList();
        this.admPoliciesButton = new ButtonItem(save.getGame().getLocalisationClean("HEADER_TECHNOLOGY", Eu4Language.getDefault()), null,
                                                save.getGame().getLocalisationClean("POLICYVIEW_ADMINISTRATIVE", Eu4Language.getDefault()), 2);

        this.dipPolicies = FXCollections.observableArrayList();
        this.dipPoliciesButton = new ButtonItem(save.getGame().getLocalisationClean("HEADER_TECHNOLOGY", Eu4Language.getDefault()), null,
                                                save.getGame().getLocalisationClean("POLICYVIEW_DIPLOMATIC", Eu4Language.getDefault()), 2);

        this.milPolicies = FXCollections.observableArrayList();
        this.milPoliciesButton = new ButtonItem(save.getGame().getLocalisationClean("HEADER_TECHNOLOGY", Eu4Language.getDefault()), null,
                                                save.getGame().getLocalisationClean("POLICYVIEW_MILITARY", Eu4Language.getDefault()), 2);

        //Modifiers
        this.modifiers = FXCollections.observableArrayList();
        this.modifiersButton = new ButtonItem(save.getGame().getLocalisationClean("DOMESTIC_MODIFIERS", Eu4Language.getDefault()), null,
                                              save.getGame().getLocalisationClean("DOMESTIC_MODIFIERS", Eu4Language.getDefault()));

        //Flags
        this.flags = FXCollections.observableArrayList();
        this.flagsButton = new ButtonItem(SheetCategory.COUNTRY_FLAGS, null,
                                          SheetCategory.COUNTRY_FLAGS.getForDefaultLocale());

        this.hiddenFlags = FXCollections.observableArrayList();
        this.hiddenFlagsButton = new ButtonItem(SheetCategory.COUNTRY_FLAGS, null,
                                                SheetCategory.COUNTRY_HIDDEN_FLAGS.getForDefaultLocale());

        this.validationSupport = new ValidationSupport();
        this.validationSupport.registerValidator(this.nameField.getTextField(), Validator.createEmptyValidator("Text is required"));
        this.validationSupport.setValidationDecorator(new CompoundValidationDecoration(new CustomGraphicValidationDecoration(),
                                                                                       new StyleClassValidationDecoration("validation-error", null)));
    }

    public void update(SaveCountry country) {
        update(country, false);
    }

    public void update(SaveCountry country, boolean force) {
        this.colorChanged.set(false);

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
                this.nameField.setValue(CountryStringConverter.INSTANCE.toString(this.country));
                this.nameField.setSupplier(() -> CountryStringConverter.INSTANCE.toString(this.country));
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

                this.capitalField.getChoices().setAll(this.country.getOwnedProvinces()
                                                                  .stream()
                                                                  .sorted(Comparator.comparing(SaveProvince::getName, Eu4Utils.COLLATOR))
                                                                  .collect(Collectors.toList()));
                this.capitalField.setValue(this.country.getCapital());
                this.capitalField.setSupplier(this.country::getCapital);
                items.add(this.capitalField);

                this.mapColorField.setValue(Eu4SaveEditorUtils.countryToMapColor(country));
                items.add(this.mapColorField);

                //Culture
                this.cultureField.setValue(this.country.getPrimaryCulture());
                this.cultureField.setSupplier(this.country::getPrimaryCulture);
                items.add(this.cultureField);

                this.acceptedCulturesField.setValue(FXCollections.observableArrayList(this.country.getAcceptedCultures()));
                this.acceptedCulturesField.setSupplier(this.country::getAcceptedCultures);
                items.add(this.acceptedCulturesField);

                //Religion
                this.religionField.setValue(this.country.getReligion());
                this.religionField.setSupplier(this.country::getReligion);
                items.add(this.religionField);

                if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().isUseAuthority()) {
                    this.authorityField.setValue(this.country.getAuthority());
                    this.authorityField.setSupplier(this.country::getAuthority);
                    items.add(this.authorityField);
                }

                if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().isUseAuthority()) {
                    this.authorityField.setValue(this.country.getAuthority());
                    this.authorityField.setSupplier(this.country::getAuthority);
                    items.add(this.authorityField);
                }

                if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().useReligiousReforms()) {
                    this.passedReligiousReforms = FXCollections.observableArrayList(this.country.getReligiousReforms().getAdoptedReforms());
                    this.notPassedReligiousReforms = this.country.getSave()
                                                                 .getGame()
                                                                 .getReligiousReforms(this.country.getReligiousReforms().getReligiousReforms().getName())
                                                                 .getReforms()
                                                                 .stream()
                                                                 .filter(Predicate.not(this.passedReligiousReforms::contains))
                                                                 .collect(Collectors.toCollection(FXCollections::observableArrayList));
                    this.religiousReformsButton.getButton().setOnAction(event -> {
                        CustomListSelectionView<ReligiousReform> listSelectionView = new CustomListSelectionView<>(
                                FXCollections.observableArrayList(this.notPassedReligiousReforms),
                                FXCollections.observableArrayList(this.passedReligiousReforms),
                                new ReligiousReformStringCellFactory(this.country.getSave().getGame()),
                                750, 600);

                        ListSelectionViewDialog<ReligiousReform> dialog = new ListSelectionViewDialog<>(this.country.getSave(),
                                                                                                        listSelectionView,
                                                                                                        this.country.getSave()
                                                                                                                    .getGame()
                                                                                                                    .getLocalisationClean("CELESTIAL_DECISIONS", Eu4Language.getDefault()),
                                                                                                        () -> this.notPassedReligiousReforms,
                                                                                                        () -> this.passedReligiousReforms);

                        Optional<List<ReligiousReform>> reforms = dialog.showAndWait();

                        reforms.ifPresent(religiousReforms -> {
                            this.passedReligiousReforms.setAll(religiousReforms);
                            this.notPassedReligiousReforms.setAll(this.country.getSave()
                                                                              .getGame()
                                                                              .getReligiousReforms(
                                                                                      this.country.getReligiousReforms().getReligiousReforms().getName())
                                                                              .getReforms()
                                                                              .stream()
                                                                              .filter(Predicate.not(this.passedReligiousReforms::contains))
                                                                              .toArray(ReligiousReform[]::new));
                        });
                    });

                    items.add(this.religiousReformsButton);
                }

                if (this.country.getReligion() != null &&
                    (this.country.getReligion().getGameReligion().usesAnglicanPower()
                     || this.country.getReligion().getGameReligion().usesHussitePower()
                     || this.country.getReligion().getGameReligion().usesChurchPower())) {
                    this.churchPowerField.setValue(this.country.getChurch().getPower());
                    this.churchPowerField.setSupplier(() -> this.country.getChurch().getPower());
                    items.add(this.churchPowerField);
                }

                if (this.country.getReligion() != null && CollectionUtils.isNotEmpty(this.country.getReligion().getGameReligion().getAspects())) {
                    //Todo
                }

                if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().useFervor()) {
                    this.fervorField.setValue(this.country.getFervor().getValue());
                    this.fervorField.setSupplier(() -> this.country.getFervor().getValue());
                    items.add(this.fervorField);
                }

                if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().hasPatriarchs()) {
                    this.patriarchAuthorityField.setValue(this.country.getPatriarchAuthority());
                    this.patriarchAuthorityField.setSupplier(() -> this.country.getPatriarchAuthority());
                    items.add(this.patriarchAuthorityField);
                }

                if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().useFetishistCult()) {
                    this.fetishistCultField.getChoices()
                                           .setAll(Stream.concat(Stream.of((FetishistCult) null), this.country.getUnlockedFetishistCults().stream())
                                                         .sorted(Comparator.nullsFirst(
                                                                 Comparator.comparing(f -> Eu4SaveEditorUtils.localize(f.getName(),
                                                                                                                       this.country.getSave().getGame()),
                                                                                      Eu4Utils.COLLATOR)))
                                                         .toArray(FetishistCult[]::new));
                    this.fetishistCultField.setValue(this.country.getFetishistCult());
                    this.fetishistCultField.setSupplier(this.country::getFetishistCult);
                    items.add(this.fetishistCultField);
                }

                if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().usesIsolationism()) {
                    this.isolationismField.setSupplier(this.country::getIsolationismLevel);
                    this.isolationismField.setValue(this.country.getIsolationismLevel());
                    items.add(this.isolationismField);
                }

                if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().usesKarma()) {
                    this.karmaField.setValue(this.country.getKarma());
                    this.karmaField.setSupplier(() -> this.country.getKarma());
                    items.add(this.karmaField);
                }

                if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().usePersonalDeity()) {
                    this.personalDeityField.getChoices()
                                           .setAll(Stream.concat(Stream.of((PersonalDeity) null), this.country.getUnlockedPersonalDeities().stream())
                                                         .sorted(Comparator.nullsFirst(
                                                                 Comparator.comparing(p -> Eu4SaveEditorUtils.localize(p.getName(),
                                                                                                                       this.country.getSave().getGame()),
                                                                                      Eu4Utils.COLLATOR)))
                                                         .toArray(PersonalDeity[]::new));
                    this.personalDeityField.setValue(this.country.getPersonalDeity());
                    this.personalDeityField.setSupplier(this.country::getPersonalDeity);
                    items.add(this.personalDeityField);
                }

                if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().usesPiety()) {
                    this.pietyField.setValue(this.country.getPiety());
                    this.pietyField.setSupplier(() -> this.country.getPiety());
                    items.add(this.pietyField);
                }

                if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().usesHarmony()) {
                    this.harmonyField.setValue(this.country.getHarmony());
                    this.harmonyField.setSupplier(() -> this.country.getHarmony());
                    items.add(this.harmonyField);

                    this.harmonizedReligionGroups = FXCollections.observableArrayList(this.country.getHarmonizedReligionGroups());
                    this.notHarmonizedReligionGroups = this.country.getSave()
                                                                   .getGame()
                                                                   .getReligionGroups()
                                                                   .stream()
                                                                   .filter(religionGroup -> StringUtils.isNotBlank(religionGroup.getHarmonizedModifier()))
                                                                   .filter(Predicate.not(this.harmonizedReligionGroups::contains))
                                                                   .collect(Collectors.toCollection(FXCollections::observableArrayList));
                    this.harmonizedReligionGroupsButton.getButton().setOnAction(event -> {
                        CustomListSelectionView<ReligionGroup> listSelectionView = new CustomListSelectionView<>(
                                FXCollections.observableArrayList(this.notHarmonizedReligionGroups),
                                FXCollections.observableArrayList(this.harmonizedReligionGroups),
                                new ReligionGroupStringCellFactory(this.country.getSave().getGame()),
                                750, 600);

                        ListSelectionViewDialog<ReligionGroup> dialog =
                                new ListSelectionViewDialog<>(this.country.getSave(),
                                                              listSelectionView,
                                                              this.country.getSave()
                                                                          .getGame()
                                                                          .getLocalisationClean("HARMONIZED_RELIGION_GROUP", Eu4Language.getDefault()),
                                                              () -> this.notHarmonizedReligionGroups,
                                                              () -> this.harmonizedReligionGroups);

                        Optional<List<ReligionGroup>> reforms = dialog.showAndWait();

                        reforms.ifPresent(religiousReforms -> {
                            this.harmonizedReligionGroups.setAll(religiousReforms);
                            this.notHarmonizedReligionGroups.setAll(this.country.getSave()
                                                                                .getGame()
                                                                                .getReligionGroups()
                                                                                .stream()
                                                                                .filter(religionGroup -> StringUtils.isNotBlank(religionGroup.getHarmonizedModifier()))
                                                                                .filter(Predicate.not(this.harmonizedReligionGroups::contains))
                                                                                .toArray(ReligionGroup[]::new));
                        });
                    });

                    items.add(this.harmonizedReligionGroupsButton);

                    this.harmonizedReligions = FXCollections.observableArrayList(this.country.getHarmonizedReligions()
                                                                                             .stream()
                                                                                             .map(SaveReligion::getGameReligion)
                                                                                             .collect(Collectors.toList()));
                    this.notHarmonizedReligions = this.country.getSave()
                                                              .getGame()
                                                              .getReligions()
                                                              .stream()
                                                              .filter(religion -> StringUtils.isNotBlank(religion.getHarmonizedModifier()))
                                                              .filter(Predicate.not(this.harmonizedReligions::contains))
                                                              .collect(Collectors.toCollection(FXCollections::observableArrayList));
                    this.harmonizedReligionsButton.getButton().setOnAction(event -> {
                        CustomListSelectionView<Religion> listSelectionView = new CustomListSelectionView<>(
                                FXCollections.observableArrayList(this.notHarmonizedReligions),
                                FXCollections.observableArrayList(this.harmonizedReligions),
                                new ReligionStringCellFactory(this.country.getSave().getGame()),
                                750, 600);

                        ListSelectionViewDialog<Religion> dialog =
                                new ListSelectionViewDialog<>(this.country.getSave(),
                                                              listSelectionView,
                                                              this.country.getSave()
                                                                          .getGame()
                                                                          .getLocalisationClean("HARMONIZED_RELIGION_", Eu4Language.getDefault()),
                                                              () -> this.notHarmonizedReligions,
                                                              () -> this.harmonizedReligions);

                        Optional<List<Religion>> reforms = dialog.showAndWait();

                        reforms.ifPresent(religiousReforms -> {
                            this.harmonizedReligions.setAll(religiousReforms);
                            this.notHarmonizedReligions.setAll(this.country.getSave()
                                                                           .getGame()
                                                                           .getReligions()
                                                                           .stream()
                                                                           .filter(religion -> StringUtils.isNotBlank(religion.getHarmonizedModifier()))
                                                                           .filter(Predicate.not(this.harmonizedReligions::contains))
                                                                           .toArray(Religion[]::new));
                        });
                    });

                    items.add(this.harmonizedReligionsButton);
                }

                if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().useDoom()) {
                    this.doomField.setValue(this.country.getDoom());
                    this.doomField.setSupplier(() -> this.country.getDoom());
                    items.add(this.doomField);
                }

                if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().canHaveSecondaryReligion()) {
                    this.secondaryReligionsField.getChoices()
                                                .setAll(Stream.concat(Stream.of((SaveReligion) null),
                                                                      this.country.getAvailableSecondaryReligions()
                                                                                  .stream()
                                                                                  .filter(religion -> !religion.equals(this.country.getReligion())))
                                                              .sorted(Comparator.nullsFirst(
                                                                      Comparator.comparing(r -> Eu4SaveEditorUtils.localize(r.getName(),
                                                                                                                            this.country.getSave().getGame()),
                                                                                           Eu4Utils.COLLATOR)))
                                                              .toArray(SaveReligion[]::new));
                    this.secondaryReligionsField.setValue(this.country.getSecondaryReligion());
                    this.secondaryReligionsField.setSupplier(this.country::getSecondaryReligion);
                    items.add(this.secondaryReligionsField);
                }

                if (this.country.getReligion() != null && CollectionUtils.isNotEmpty(this.country.getReligion().getGameReligion().getIcons())) {
                    //Todo
                }

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
                                                                         this.country.getSave()
                                                                                     .getGame()
                                                                                     .getLocalisationClean("AI_LOANS", Eu4Language.getDefault()),
                                                                         (list) -> new Loan(300, 4, this.country.getSave().getDate().plusYears(5)),
                                                                         () -> this.loans);
                    Optional<List<Loan>> countrySubjects = dialog.showAndWait();

                    countrySubjects.ifPresent(this.loans::setAll);
                });
                items.add(this.loansButton);

                //Institutions
                this.institutionsEmbracedFields.clear();
                for (int i = 0; i < this.country.getSave().getInstitutions().getNbInstitutions(); i++) {
                    CheckBoxItem checkBoxItem = new CheckBoxItem(Eu4SaveEditorUtils.localize("INSTITUTIONS", this.country.getSave().getGame()),
                                                                 Eu4SaveEditorUtils.localize(this.country.getSave().getGame().getInstitution(i).getName(),
                                                                                             this.country.getSave().getGame()),
                                                                 this.country.getEmbracedInstitution(i));
                    this.institutionsEmbracedFields.add(checkBoxItem);
                }
                items.addAll(this.institutionsEmbracedFields);

                //Government
                this.governmentRankField.getChoices()
                                        .setAll(this.country.getGovernmentName()
                                                            .getRanks()
                                                            .entrySet()
                                                            .stream()
                                                            .map(e -> Pair.of(e.getKey(), Eu4SaveEditorUtils.localize(e.getValue(),
                                                                                                                      this.country.getSave().getGame())))
                                                            .toList());
                this.governmentRankField.setValue(Pair.of(this.country.getGovernmentLevel(),
                                                          Eu4SaveEditorUtils.localize(this.country.getGovernmentName()
                                                                                                  .getRank(this.country.getGovernmentLevel()),
                                                                                      this.country.getSave().getGame())));
                this.governmentRankField.setSupplier(() -> Pair.of(this.country.getGovernmentLevel(),
                                                                   Eu4SaveEditorUtils.localize(this.country.getGovernmentName()
                                                                                                           .getRank(this.country.getGovernmentLevel()),
                                                                                               this.country.getSave().getGame())));
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
                                                                          this.country.getSave()
                                                                                      .getGame()
                                                                                      .getLocalisationClean("CURRENT_MONARCH", Eu4Language.getDefault()),
                                                                          this.cultures, this.religions);

                    if (!sheet.getPropertySheet().getItems().isEmpty()) {
                        this.monarchPropertySheet = sheet;
                        this.courtPropertySheet.getItems().addAll(sheet.getPropertySheet().getItems());
                    }
                }

                this.heirPropertySheet = null;

                if (this.country.getHeir() != null) {
                    MonarchPropertySheet sheet = new MonarchPropertySheet(this.country, this.country.getHeir(),
                                                                          this.country.getSave()
                                                                                      .getGame()
                                                                                      .getLocalisationClean("HEIR", Eu4Language.getDefault()),
                                                                          this.cultures, this.religions);

                    if (!sheet.getPropertySheet().getItems().isEmpty()) {
                        this.heirPropertySheet = sheet;
                        this.courtPropertySheet.getItems().addAll(sheet.getPropertySheet().getItems());
                    }
                }

                this.queenPropertySheet = null;

                if (this.country.getQueen() != null) {
                    MonarchPropertySheet sheet = new MonarchPropertySheet(this.country, this.country.getQueen(),
                                                                          this.country.getSave()
                                                                                      .getGame()
                                                                                      .getLocalisationClean("CONSORT", Eu4Language.getDefault()),
                                                                          this.cultures, this.religions);

                    if (!sheet.getPropertySheet().getItems().isEmpty()) {
                        this.queenPropertySheet = sheet;
                        this.courtPropertySheet.getItems().addAll(sheet.getPropertySheet().getItems());
                    }
                }

                if (!this.courtPropertySheet.getItems().isEmpty()) {
                    items.add(new PropertySheetItem(this.country.getSave().getGame().getLocalisationClean("CATEGORY_COURT", Eu4Language.getDefault()),
                                                    this.courtPropertySheet));
                }

                //Diplomacy
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
                                                  this.country.getSave().getGame().getLocalisationClean("HEADER_SUBJECTS", Eu4Language.getDefault()),
                                                  list -> new CountrySubject(this.country,
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

                this.rivals.setAll(this.country.getRivals().values().stream().map(rival -> new Rival(this.country, rival)).collect(Collectors.toList()));
                this.rivalsButton.getButton().setOnAction(event -> {
                    TableView2Rival tableView2Rival = new TableView2Rival(this.country, this.rivals, this.countriesAlive);
                    TableViewDialog<Rival> dialog = new TableViewDialog<>(this.country.getSave(),
                                                                          tableView2Rival,
                                                                          this.country.getSave()
                                                                                      .getGame()
                                                                                      .getLocalisationClean("RIVALS", Eu4Language.getDefault()),
                                                                          list -> new Rival(this.country,
                                                                                            this.countriesAlive.stream()
                                                                                                               .filter(c -> list.stream()
                                                                                                                                .noneMatch(r -> c.equals(
                                                                                                                                        r.getTarget())))
                                                                                                               .findFirst()
                                                                                                               .get(),
                                                                                            this.country.getSave().getDate()),

                                                                          () -> this.rivals);
                    dialog.setDisableAddProperty(tableView2Rival.disableAddPropertyProperty());
                    Optional<List<Rival>> rivalList = dialog.showAndWait();

                    rivalList.ifPresent(this.rivals::setAll);
                });
                items.add(this.rivalsButton);

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

                this.leaders.setAll(this.country.getLeaders().values().stream().map(Leader::new).collect(Collectors.toList()));
                this.leadersButton.getButton().setOnAction(event -> {
                    TableViewDialog<Leader> dialog = new TableViewDialog<>(this.country.getSave(),
                                                                           new TableView2Leader(this.country, this.leaders),
                                                                           this.country.getSave()
                                                                                       .getGame()
                                                                                       .getLocalisationClean("HEADER_LEADER", Eu4Language.getDefault()),
                                                                           list -> new Leader(this.country,
                                                                                              "New one",
                                                                                              LeaderType.GENERAL,
                                                                                              0,
                                                                                              0,
                                                                                              0,
                                                                                              0,
                                                                                              null,
                                                                                              this.country.getSave()
                                                                                                          .getDate()
                                                                                                          .minusYears(this.country.getSave()
                                                                                                                                  .getGame()
                                                                                                                                  .getAgeOfAdulthood())),
                                                                           () -> this.leaders);
                    Optional<List<Leader>> countrySubjects = dialog.showAndWait();

                    countrySubjects.ifPresent(this.leaders::setAll);
                });
                items.add(this.leadersButton);

                //Estates
                this.estatePropertySheets.clear();
                this.estatesPropertySheet.getItems().clear();
                this.country.getEstates()
                            .stream()
                            .sorted(Comparator.comparing(e -> Eu4SaveEditorUtils.localize(e.getEstateGame().getName(), this.country.getSave().getGame()),
                                                         Eu4Utils.COLLATOR))
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
                    items.add(new PropertySheetItem(this.country.getSave().getGame().getLocalisationClean("HEADER_ESTATES", Eu4Language.getDefault()),
                                                    this.estatesPropertySheet));
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
                                                  this.country.getSave().getGame().getLocalisationClean("HEADER_IDEAS", Eu4Language.getDefault()),
                                                  list -> new Idea(this.country.getSave()
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

                this.availableAdmPolicies = this.country.getSave()
                                                        .getGame()
                                                        .getPolicies()
                                                        .stream()
                                                        .filter(policy -> Power.ADM.equals(policy.getCategory()))
                                                        .filter(policy -> policy.getAllow().apply(this.country, this.country))
                                                        .collect(Collectors.toCollection(FXCollections::observableArrayList));

                if (!availableAdmPolicies.isEmpty()) {
                    this.admPolicies.setAll(this.country.getActivePolicies()
                                                        .stream()
                                                        .filter(policy -> Power.ADM.equals(policy.getPolicy().getCategory()))
                                                        .map(ActivePolicy::new)
                                                        .toArray(ActivePolicy[]::new));
                    this.admPoliciesButton.getButton().setOnAction(event -> {
                        TableView2Policy tableView2Policy = new TableView2Policy(this.country, this.admPolicies, availableAdmPolicies, "POSSIBLE_ADM_POLICY");
                        TableViewDialog<ActivePolicy> dialog =
                                new TableViewDialog<>(this.country.getSave(),
                                                      tableView2Policy,
                                                      this.country.getSave()
                                                                  .getGame()
                                                                  .getLocalisationClean("POLICYVIEW_ADMINISTRATIVE", Eu4Language.getDefault()),
                                                      list -> new ActivePolicy(availableAdmPolicies.stream()
                                                                                                   .filter(policy -> list.stream()
                                                                                                                         .noneMatch(
                                                                                                                                 activePolicy -> activePolicy.getPolicy()
                                                                                                                                                             .equals(policy)))
                                                                                                   .findFirst()
                                                                                                   .get(),
                                                                               this.country.getSave().getDate()),
                                                      () -> this.admPolicies);
                        dialog.setDisableAddProperty(tableView2Policy.disableAddPropertyProperty());
                        Optional<List<ActivePolicy>> policyList = dialog.showAndWait();

                        policyList.ifPresent(this.admPolicies::setAll);
                    });
                    items.add(this.admPoliciesButton);
                }

                this.availableDipPolicies = this.country.getSave()
                                                        .getGame()
                                                        .getPolicies()
                                                        .stream()
                                                        .filter(policy -> Power.DIP.equals(policy.getCategory()))
                                                        .filter(policy -> policy.getAllow().apply(this.country, this.country))
                                                        .collect(Collectors.toCollection(FXCollections::observableArrayList));

                if (!availableDipPolicies.isEmpty()) {
                    this.dipPolicies.setAll(this.country.getActivePolicies()
                                                        .stream()
                                                        .filter(policy -> Power.DIP.equals(policy.getPolicy().getCategory()))
                                                        .map(ActivePolicy::new)
                                                        .toArray(ActivePolicy[]::new));
                    this.dipPoliciesButton.getButton().setOnAction(event -> {
                        TableView2Policy tableView2Policy = new TableView2Policy(this.country, this.dipPolicies, availableDipPolicies, "POSSIBLE_DIP_POLICY");
                        TableViewDialog<ActivePolicy> dialog =
                                new TableViewDialog<>(this.country.getSave(),
                                                      tableView2Policy,
                                                      this.country.getSave().getGame().getLocalisationClean("POLICYVIEW_DIPLOMATIC", Eu4Language.getDefault()),
                                                      list -> new ActivePolicy(availableDipPolicies.stream()
                                                                                                   .filter(policy -> list.stream()
                                                                                                                         .noneMatch(
                                                                                                                                 activePolicy -> activePolicy.getPolicy()
                                                                                                                                                             .equals(policy)))
                                                                                                   .findFirst()
                                                                                                   .get(),
                                                                               this.country.getSave().getDate()),
                                                      () -> this.dipPolicies);
                        dialog.setDisableAddProperty(tableView2Policy.disableAddPropertyProperty());
                        Optional<List<ActivePolicy>> policyList = dialog.showAndWait();

                        policyList.ifPresent(this.dipPolicies::setAll);
                    });
                    items.add(this.dipPoliciesButton);
                }

                this.availableMilPolicies = this.country.getSave()
                                                        .getGame()
                                                        .getPolicies()
                                                        .stream()
                                                        .filter(policy -> Power.MIL.equals(policy.getCategory()))
                                                        .filter(policy -> policy.getAllow().apply(this.country, this.country))
                                                        .collect(Collectors.toCollection(FXCollections::observableArrayList));

                if (!availableMilPolicies.isEmpty()) {
                    this.milPolicies.setAll(this.country.getActivePolicies()
                                                        .stream()
                                                        .filter(policy -> Power.MIL.equals(policy.getPolicy().getCategory()))
                                                        .map(ActivePolicy::new)
                                                        .toArray(ActivePolicy[]::new));
                    this.milPoliciesButton.getButton().setOnAction(event -> {
                        TableView2Policy tableView2Policy = new TableView2Policy(this.country, this.milPolicies, availableMilPolicies, "POSSIBLE_MIL_POLICY");
                        TableViewDialog<ActivePolicy> dialog =
                                new TableViewDialog<>(this.country.getSave(),
                                                      tableView2Policy,
                                                      this.country.getSave().getGame().getLocalisationClean("POLICYVIEW_MILITARY", Eu4Language.getDefault()),
                                                      list -> new ActivePolicy(availableMilPolicies.stream()
                                                                                                   .filter(policy -> list.stream()
                                                                                                                         .noneMatch(
                                                                                                                                 activePolicy -> activePolicy.getPolicy()
                                                                                                                                                             .equals(policy)))
                                                                                                   .findFirst()
                                                                                                   .get(),
                                                                               this.country.getSave().getDate()),
                                                      () -> this.milPolicies);
                        dialog.setDisableAddProperty(tableView2Policy.disableAddPropertyProperty());
                        Optional<List<ActivePolicy>> policyList = dialog.showAndWait();

                        policyList.ifPresent(this.milPolicies::setAll);
                    });
                    items.add(this.milPoliciesButton);
                }

                //Modifiers
                this.modifiers.setAll(this.country.getModifiers().stream().map(Modifier::new).collect(Collectors.toList()));
                this.modifiersButton.getButton().setOnAction(event -> {
                    TableView2Modifier tableView2Modifier = new TableView2Modifier(this.country.getSave(), this.modifiers);
                    TableViewDialog<Modifier> dialog = new TableViewDialog<>(this.country.getSave(),
                                                                             tableView2Modifier,
                                                                             this.country.getSave()
                                                                                         .getGame()
                                                                                         .getLocalisationClean("DOMESTIC_MODIFIERS", Eu4Language.getDefault()),
                                                                             list -> null,
                                                                             () -> this.modifiers);
                    dialog.setDisableAddProperty(new SimpleBooleanProperty(true));
                    Optional<List<Modifier>> modifierList = dialog.showAndWait();

                    modifierList.ifPresent(this.modifiers::setAll);
                });
                items.add(this.modifiersButton);

                //Flags
                if (this.country.getFlags() != null) {
                    this.flags.setAll(this.country.getFlags()
                                                  .getAll()
                                                  .entrySet()
                                                  .stream()
                                                  .map(StringDate::new)
                                                  .sorted(Comparator.comparing(StringDate::getDate))
                                                  .collect(Collectors.toList()));
                    this.flagsButton.getButton().setOnAction(event -> {
                        TableView2StringDate tableView2Flag = new TableView2StringDate(this.country.getSave(), this.flags, false, null, null);
                        TableViewDialog<StringDate> dialog = new TableViewDialog<>(this.country.getSave(),
                                                                                   tableView2Flag,
                                                                                   SheetCategory.COUNTRY_FLAGS.getForDefaultLocale(),
                                                                                   list -> null,
                                                                                   () -> this.flags);
                        dialog.setDisableAddProperty(new SimpleBooleanProperty(true));
                        Optional<List<StringDate>> flagList = dialog.showAndWait();

                        flagList.ifPresent(this.flags::setAll);
                    });
                    items.add(this.flagsButton);
                } else {
                    this.flags.clear();
                }

                if (this.country.getHiddenFlags() != null) {
                    this.hiddenFlags.setAll(this.country.getHiddenFlags()
                                                        .getAll()
                                                        .entrySet()
                                                        .stream()
                                                        .map(StringDate::new)
                                                        .sorted(Comparator.comparing(StringDate::getDate))
                                                        .collect(Collectors.toList()));
                    this.hiddenFlagsButton.getButton().setOnAction(event -> {
                        TableView2StringDate tableView2HiddenFlag = new TableView2StringDate(this.country.getSave(), this.hiddenFlags, false, null, null);
                        TableViewDialog<StringDate> dialog = new TableViewDialog<>(this.country.getSave(),
                                                                                   tableView2HiddenFlag,
                                                                                   SheetCategory.COUNTRY_HIDDEN_FLAGS.getForDefaultLocale(),
                                                                                   list -> null,
                                                                                   () -> this.hiddenFlags);
                        dialog.setDisableAddProperty(new SimpleBooleanProperty(true));
                        Optional<List<StringDate>> hiddenFlagList = dialog.showAndWait();

                        hiddenFlagList.ifPresent(this.hiddenFlags::setAll);
                    });
                    items.add(this.hiddenFlagsButton);
                } else {
                    this.hiddenFlags.clear();
                }

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

        if (!Objects.equals(this.country.getInflation(), this.inflationField.getTrueValue())) {
            this.country.setInflation(this.inflationField.getTrueValue());
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

        if (!Objects.deepEquals(this.country.getCapital(), this.capitalField.getSelectedValue())) {
            this.country.setCapital(this.capitalField.getSelectedValue());
        }

        if (!Objects.deepEquals(Eu4SaveEditorUtils.countryToMapColor(this.country), this.mapColorField.getValue())) {
            this.country.getColors().setMapColor((int) (this.mapColorField.getSelectedValue().getRed() * 255),
                                                 (int) (this.mapColorField.getSelectedValue().getGreen() * 255),
                                                 (int) (this.mapColorField.getSelectedValue().getBlue() * 255));
            this.colorChanged.set(true);
        }

        if (!Objects.deepEquals(this.country.getPrimaryCulture(), this.cultureField.getSelectedValue())) {
            this.country.setPrimaryCulture(this.cultureField.getSelectedValue());
        }

        if (!Objects.deepEquals(this.country.getAcceptedCultures(), this.acceptedCulturesField.getSelectedValues())) {
            this.country.setAcceptedCulture(new ArrayList<>(this.acceptedCulturesField.getSelectedValues()));
        }

        if (!Objects.deepEquals(this.country.getReligion(), this.religionField.getSelectedValue())) {
            this.country.setReligion(this.religionField.getSelectedValue());
        }

        if (this.country.getReligion() != null && BooleanUtils.isTrue(this.country.getReligion().getGameReligion().isUseAuthority())) {
            if (!Objects.equals(this.country.getAuthority(), this.authorityField.getDoubleValue())) {
                this.country.setAuthority(this.authorityField.getDoubleValue());
            }
        }

        if (this.country.getReligion() != null
            && (this.country.getReligion().getGameReligion().usesAnglicanPower() || this.country.getReligion().getGameReligion().usesHussitePower()
                || this.country.getReligion().getGameReligion().usesChurchPower())) {
            if (!Objects.equals(this.country.getChurch().getPower(), this.churchPowerField.getDoubleValue())) {
                this.country.getChurch().setPower(this.churchPowerField.getDoubleValue());
            }
        }

        if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().useReligiousReforms()) {
            if (!Objects.deepEquals(this.country.getReligiousReforms().getAdoptedReforms(), this.passedReligiousReforms)) {
                this.country.getReligiousReforms()
                            .getAdoptedReforms()
                            .forEach(reform -> this.passedReligiousReforms.stream()
                                                                          .filter(reform::equals)
                                                                          .findFirst()
                                                                          .ifPresentOrElse(this.passedReligiousReforms::remove,
                                                                                           () -> this.country.getReligiousReforms()
                                                                                                             .removeAdoptedReform(reform)));
                this.passedReligiousReforms.forEach(reform -> this.country.getReligiousReforms().addAdoptedReform(reform));
            }
        }

        if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().useFervor()) {
            if (!Objects.equals(this.country.getFervor().getValue(), this.fervorField.getDoubleValue())) {
                this.country.getFervor().setValue(this.fervorField.getDoubleValue());
            }
        }

        if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().hasPatriarchs()) {
            if (!Objects.equals(this.country.getPatriarchAuthority(), this.patriarchAuthorityField.getDoubleValue())) {
                this.country.setPatriarchAuthority(this.patriarchAuthorityField.getDoubleValue());
            }
        }

        if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().useFetishistCult()) {
            if (!Objects.deepEquals(this.country.getFetishistCult(), this.fetishistCultField.getSelectedValue())) {
                this.country.setFetishistCult(this.fetishistCultField.getSelectedValue());
            }
        }

        if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().usesIsolationism()) {
            if (!Objects.equals(this.country.getIsolationismLevel(), this.isolationismField.getTrueValue())) {
                this.country.setIsolationismLevel(this.isolationismField.getTrueValue());
            }
        }

        if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().usesKarma()) {
            if (!Objects.equals(this.country.getKarma(), this.karmaField.getIntValue())) {
                this.country.setKarma(this.karmaField.getIntValue());
            }
        }

        if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().usesPiety()) {
            if (!Objects.equals(this.country.getPiety(), this.pietyField.getDoubleValue())) {
                this.country.setPiety(this.pietyField.getDoubleValue());
            }
        }

        if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().usesHarmony()) {
            if (!Objects.equals(this.country.getHarmony(), this.harmonyField.getDoubleValue())) {
                this.country.setHarmony(this.harmonyField.getDoubleValue());
            }

            if (!Objects.deepEquals(this.country.getHarmonizedReligionGroups(), this.harmonizedReligionGroups)) {
                if (CollectionUtils.isNotEmpty(this.country.getHarmonizedReligionGroups())) {
                    this.country.getHarmonizedReligionGroups()
                                .forEach(group -> this.harmonizedReligionGroups.stream()
                                                                               .filter(group::equals)
                                                                               .findFirst()
                                                                               .ifPresentOrElse(this.harmonizedReligionGroups::remove,
                                                                                                () -> this.country.removeHarmonizedReligionGroup(group)));
                }

                this.harmonizedReligionGroups.forEach(religionGroup -> this.country.addHarmonizedReligionGroup(religionGroup));
            }

            if (!Objects.deepEquals(this.country.getHarmonizedReligions(), this.harmonizedReligions)) {
                if (CollectionUtils.isNotEmpty(this.country.getHarmonizedReligions())) {
                    this.country.getHarmonizedReligions()
                                .stream()
                                .map(SaveReligion::getGameReligion)
                                .forEach(religion -> this.harmonizedReligions.stream()
                                                                             .filter(religion::equals)
                                                                             .findFirst()
                                                                             .ifPresentOrElse(this.harmonizedReligions::remove,
                                                                                              () -> this.country.removeHarmonizedReligion(religion)));
                }

                this.harmonizedReligions.forEach(religion -> this.country.addHarmonizedReligion(religion));
            }
        }

        if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().usePersonalDeity()) {
            if (!Objects.deepEquals(this.country.getPersonalDeity(), this.personalDeityField.getSelectedValue())) {
                this.country.setPersonalDeity(this.personalDeityField.getSelectedValue());
            }
        }

        if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().useDoom()) {
            if (!Objects.equals(this.country.getDoom(), this.doomField.getDoubleValue())) {
                this.country.setDoom(this.doomField.getDoubleValue());
            }
        }

        if (this.country.getReligion() != null && this.country.getReligion().getGameReligion().canHaveSecondaryReligion()) {
            if (!Objects.deepEquals(this.country.getSecondaryReligion(), this.secondaryReligionsField.getSelectedValue())) {
                this.country.setSecondaryReligion(this.secondaryReligionsField.getSelectedValue());
            }
        }

        if (!Objects.equals(this.country.getGovernmentReformProgress(), this.governmentReformProgressField.getTrueValue())) {
            this.country.setGovernmentReformProgress(this.governmentReformProgressField.getTrueValue());
        }

        for (int i = 0; i < this.country.getSave().getInstitutions().getNbInstitutions(); i++) {
            if (this.country.getEmbracedInstitution(i) != this.institutionsEmbracedFields.get(i).isSelected()) {
                this.country.embracedInstitution(i, this.institutionsEmbracedFields.get(i).isSelected());
            }
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

        if (!Objects.equals(this.country.getWarExhaustion(), this.warEhaustionField.getDoubleValue())) {
            this.country.setWarExhaustion(this.warEhaustionField.getDoubleValue());
        }

        if (this.country.getLeaders().size() != this.leaders.size() || this.leaders.stream().anyMatch(Leader::isChanged)) {
            this.country.getLeaders()
                        .values()
                        .forEach(leader -> this.leaders.stream()
                                                       .filter(l -> leader.getId().getId().equals(l.getId()))
                                                       .findFirst()
                                                       .ifPresentOrElse(l -> {
                                                                            if (!Objects.equals(l.getBirthDate(), leader.getBirthDate())) {
                                                                                leader.setBirthDate(l.getBirthDate());
                                                                            }

                                                                            if (!Objects.equals(l.getName(), leader.getName())) {
                                                                                leader.setName(l.getName());
                                                                            }

                                                                            if (!Objects.equals(l.getType(), leader.getType())) {
                                                                                leader.setType(l.getType());
                                                                            }

                                                                            if (!Objects.equals(l.getManuever(), leader.getManuever())) {
                                                                                leader.setManuever(l.getManuever());
                                                                            }

                                                                            if (!Objects.equals(l.getFire(), leader.getFire())) {
                                                                                leader.setFire(l.getFire());
                                                                            }

                                                                            if (!Objects.equals(l.getShock(), leader.getShock())) {
                                                                                leader.setShock(l.getShock());
                                                                            }

                                                                            if (!Objects.equals(l.getSiege(), leader.getSiege())) {
                                                                                leader.setSiege(l.getSiege());
                                                                            }

                                                                            if (!Objects.equals(l.getPersonality(), leader.getPersonality())) {
                                                                                leader.setPersonality(l.getPersonality());
                                                                            }

                                                                            this.leaders.remove(l);
                                                                        },
                                                                        () -> this.country.removeLeader(leader.getId().getId())));
            this.leaders.forEach(l -> this.country.addLeader(this.country.getSave().getDate(), l.getBirthDate(), l.getName(), l.getType(), l.getManuever(),
                                                             l.getFire(), l.getShock(), l.getSiege(), l.getPersonality()));
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

        if (this.country.getRivals().size() != this.rivals.size() || this.rivals.stream().anyMatch(Rival::isChanged)) {
            this.country.getRivals()
                        .values()
                        .forEach(rival -> this.rivals.stream()
                                                     .filter(r -> rival.getRival().equals(r.getTarget()))
                                                     .findFirst()
                                                     .ifPresentOrElse(r -> {
                                                                          if (!Objects.equals(r.getDate(), rival.getDate())) {
                                                                              rival.setDate(r.getDate());
                                                                          }

                                                                          this.rivals.remove(r);
                                                                      },
                                                                      () -> this.country.removeRival(rival.getRival())));
            this.rivals.forEach(rival -> this.country.addRival(rival.getTarget(), rival.getDate()));
        }

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

        if (this.country.getModifiers().size() != this.modifiers.size() || this.modifiers.stream().anyMatch(Modifier::isChanged)) {
            this.country.getModifiers()
                        .forEach(saveModifier -> this.modifiers.stream()
                                                               .filter(modifier -> saveModifier.getModifier().equals(modifier.getModifier()))
                                                               .findFirst()
                                                               .ifPresentOrElse(modifier -> {
                                                                                    if (!Objects.equals(modifier.getDate(), saveModifier.getDate())) {
                                                                                        saveModifier.setDate(modifier.getDate());
                                                                                    }

                                                                                    this.modifiers.remove(modifier);
                                                                                },
                                                                                () -> this.country.removeModifier(saveModifier.getModifier())));
        }

        if ((this.country.getFlags() != null && CollectionUtils.isNotEmpty(this.flags))
            || (this.country.getFlags() != null && this.country.getFlags().getAll().size() != this.flags.size())
            || this.flags.stream().anyMatch(StringDate::isChanged)) {
            this.country.getFlags()
                        .getAll()
                        .forEach((s, date) -> this.flags.stream()
                                                        .filter(flag -> s.equals(flag.getName()))
                                                        .findFirst()
                                                        .ifPresentOrElse(flag -> {
                                                                             if (!Objects.equals(flag.getDate(), date)) {
                                                                                 this.country.getFlags().set(s, flag.getDate());
                                                                             }

                                                                             this.flags.remove(flag);
                                                                         },
                                                                         () -> this.country.getFlags().remove(s)));
        }

        if ((this.country.getHiddenFlags() != null && CollectionUtils.isNotEmpty(this.hiddenFlags))
            || (this.country.getHiddenFlags() != null && this.country.getHiddenFlags().getAll().size() != this.hiddenFlags.size())
            || this.hiddenFlags.stream().anyMatch(StringDate::isChanged)) {
            this.country.getHiddenFlags()
                        .getAll()
                        .forEach((s, date) -> this.hiddenFlags.stream()
                                                              .filter(hiddenFlag -> s.equals(hiddenFlag.getName()))
                                                              .findFirst()
                                                              .ifPresentOrElse(hiddenFlag -> {
                                                                                   if (!Objects.equals(hiddenFlag.getDate(), date)) {
                                                                                       this.country.getHiddenFlags().set(s, hiddenFlag.getDate());
                                                                                   }

                                                                                   this.hiddenFlags.remove(hiddenFlag);
                                                                               },
                                                                               () -> this.country.getHiddenFlags().remove(s)));
        }

        if (CollectionUtils.isNotEmpty(this.availableAdmPolicies) &&
            (this.country.getActivePolicies().stream().filter(p -> Power.ADM.equals(p.getPolicy().getCategory())).count() != this.admPolicies.size()
             || this.admPolicies.stream().anyMatch(ActivePolicy::isChanged))) {
            this.country.getActivePolicies()
                        .stream()
                        .filter(p -> Power.ADM.equals(p.getPolicy().getCategory()))
                        .forEach(activePolicy -> this.admPolicies.stream()
                                                                 .filter(policy -> activePolicy.getPolicy().equals(policy.getPolicy()))
                                                                 .findFirst()
                                                                 .ifPresentOrElse(policy -> {
                                                                                      if (!Objects.equals(policy.getDate(), activePolicy.getDate())) {
                                                                                          activePolicy.setDate(policy.getDate());
                                                                                      }

                                                                                      this.admPolicies.remove(policy);
                                                                                  },
                                                                                  () -> this.country.removeActivePolicy(activePolicy.getPolicy())));
            this.admPolicies.forEach(policy -> this.country.addActivePolicy(policy.getPolicy(), policy.getDate()));
        }

        if (CollectionUtils.isNotEmpty(this.availableDipPolicies) &&
            (this.country.getActivePolicies().stream().filter(p -> Power.DIP.equals(p.getPolicy().getCategory())).count() != this.dipPolicies.size()
             || this.dipPolicies.stream().anyMatch(ActivePolicy::isChanged))) {
            this.country.getActivePolicies()
                        .stream()
                        .filter(p -> Power.DIP.equals(p.getPolicy().getCategory()))
                        .forEach(activePolicy -> this.dipPolicies.stream()
                                                                 .filter(policy -> activePolicy.getPolicy().equals(policy.getPolicy()))
                                                                 .findFirst()
                                                                 .ifPresentOrElse(policy -> {
                                                                                      if (!Objects.equals(policy.getDate(), activePolicy.getDate())) {
                                                                                          activePolicy.setDate(policy.getDate());
                                                                                      }

                                                                                      this.dipPolicies.remove(policy);
                                                                                  },
                                                                                  () -> this.country.removeActivePolicy(activePolicy.getPolicy())));
            this.dipPolicies.forEach(policy -> this.country.addActivePolicy(policy.getPolicy(), policy.getDate()));
        }

        if (CollectionUtils.isNotEmpty(this.availableMilPolicies) &&
            (this.country.getActivePolicies().stream().filter(p -> Power.MIL.equals(p.getPolicy().getCategory())).count() != this.milPolicies.size()
             || this.milPolicies.stream().anyMatch(ActivePolicy::isChanged))) {
            this.country.getActivePolicies()
                        .stream()
                        .filter(p -> Power.MIL.equals(p.getPolicy().getCategory()))
                        .forEach(activePolicy -> this.milPolicies.stream()
                                                                 .filter(policy -> activePolicy.getPolicy().equals(policy.getPolicy()))
                                                                 .findFirst()
                                                                 .ifPresentOrElse(policy -> {
                                                                                      if (!Objects.equals(policy.getDate(), activePolicy.getDate())) {
                                                                                          activePolicy.setDate(policy.getDate());
                                                                                      }

                                                                                      this.milPolicies.remove(policy);
                                                                                  },
                                                                                  () -> this.country.removeActivePolicy(activePolicy.getPolicy())));
            this.milPolicies.forEach(policy -> this.country.addActivePolicy(policy.getPolicy(), policy.getDate()));
        }

        update(this.country, true);
    }

    public final BooleanProperty colorChangedProperty() {
        if (this.colorChanged == null) {
            this.colorChanged = new BooleanPropertyBase() {
                @Override
                public Object getBean() {
                    return CountryPropertySheet.this;
                }

                @Override
                public String getName() {
                    return "colorChanged";
                }
            };
        }

        return colorChanged;
    }

    public SaveCountry getCountry() {
        return country;
    }

    public ValidationSupport getValidationSupport() {
        return validationSupport;
    }

    public CustomPropertySheet getPropertySheet() {
        return propertySheet;
    }
}
