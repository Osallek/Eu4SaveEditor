package fr.osallek.eu4saveeditor.controller.mapview;

import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.country.SaveCountry;
import fr.osallek.eu4parser.model.save.country.SaveEstate;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import fr.osallek.eu4saveeditor.controller.control.TableView2Privilege;
import fr.osallek.eu4saveeditor.controller.object.Privilege;
import fr.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import fr.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import fr.osallek.eu4saveeditor.controller.pane.TableViewDialog;
import fr.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ButtonItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderItem;
import fr.osallek.eu4saveeditor.controller.validator.CustomGraphicValidationDecoration;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import javafx.beans.binding.DoubleExpression;
import javafx.beans.property.DoubleProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.layout.VBox;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;

public class EstatePropertySheet extends VBox {

    private final SaveCountry country;

    private final SaveEstate estate;

    private final CustomPropertySheet propertySheet;

    private final ClearableSliderItem loyaltyField;

    private final ClearableSliderItem territoryField;

    private final ObservableList<Privilege> privileges;

    private final ValidationSupport validationSupport;

    private final List<DoubleProperty> countryEstatesTerritory = FXCollections.observableArrayList();

    public EstatePropertySheet(SaveCountry country, SaveEstate estate) {
        this.country = country;
        this.estate = estate;
        this.propertySheet = new CustomPropertySheet();
        this.propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.propertySheet.setMode(CustomPropertySheet.Mode.CATEGORY);
        this.propertySheet.setModeSwitcherVisible(false);
        this.propertySheet.setSearchBoxVisible(false);

        List<CustomPropertySheet.Item> items = new ArrayList<>();

        CustomPropertySheetSkin propertySheetSkin = new CustomPropertySheetSkin(this.propertySheet);
        this.propertySheet.setSkin(propertySheetSkin);

        this.loyaltyField = new ClearableSliderItem(Eu4SaveEditorUtils.localize(this.estate.getEstateGame().getName(), this.country.getSave().getGame()),
                                                    this.country.getSave().getGame().getLocalisationClean("LOYALTY", Eu4Language.getDefault()),
                                                    0, 100, this.estate.getLoyalty(), this.estate::getLoyalty);
        items.add(this.loyaltyField);

        this.territoryField = new ClearableSliderItem(Eu4SaveEditorUtils.localize(this.estate.getEstateGame().getName(), this.country.getSave().getGame()),
                                                      this.country.getSave().getGame().getLocalisationClean("TERRITORY", Eu4Language.getDefault()),
                                                      0, 100, this.estate.getTerritory(), this.estate::getTerritory);
        this.territoryField.getObservableDoubleValue().addListener((observable, oldValue, newValue) -> {
            if ((oldValue.doubleValue() == 100d || newValue.doubleValue() == 100d || !Objects.equals(oldValue, newValue))
                && (this.countryEstatesTerritory.stream().mapToDouble(DoubleExpression::doubleValue).sum() + newValue.doubleValue()) > 100d) {
                this.territoryField.getObservableDoubleValue().set(100d - this.countryEstatesTerritory.stream().mapToDouble(DoubleExpression::getValue).sum());
            }
        });
        items.add(this.territoryField);

        this.privileges = FXCollections.observableArrayList(this.estate.getGrantedPrivileges().stream().map(Privilege::new).collect(Collectors.toList()));
        ButtonItem privilegeButton = new ButtonItem(Eu4SaveEditorUtils.localize(this.estate.getEstateGame().getName(), this.country.getSave().getGame()), null,
                                                    country.getSave().getGame().getLocalisationClean("PRIVILEGE_PICKER_TITLE", Eu4Language.getDefault()), 2);
        privilegeButton.getButton().setOnAction(event -> {
            TableView2Privilege tableView2Privilege = new TableView2Privilege(this.country, this.estate, this.privileges,
                                                                              FXCollections.observableArrayList(
                                                                                      this.estate.getEstateGame().getPrivileges().values()));
            TableViewDialog<Privilege> dialog =
                    new TableViewDialog<>(this.country.getSave(),
                                          tableView2Privilege,
                                          this.country.getSave().getGame().getLocalisationClean("PRIVILEGE_PICKER_TITLE", Eu4Language.getDefault()),
                                          list -> new Privilege(this.estate.getEstateGame()
                                                                           .getPrivileges()
                                                                           .values()
                                                                           .stream()
                                                                           .filter(p -> list.stream().noneMatch(p2 -> p2.getPrivilege().equals(p)))
                                                                           .findFirst()
                                                                           .get(),
                                                                this.country.getSave().getDate()),
                                          () -> this.privileges.stream()
                                                               .map(Privilege::new)
                                                               .collect(Collectors.toCollection(FXCollections::observableArrayList)));
            dialog.setDisableAddProperty(tableView2Privilege.disableAddPropertyProperty());
            Optional<List<Privilege>> privilegeList = dialog.showAndWait();

            privilegeList.ifPresent(this.privileges::setAll);
        });
        items.add(privilegeButton);

        this.validationSupport = new ValidationSupport();
        this.validationSupport.setValidationDecorator(
                new CompoundValidationDecoration(new CustomGraphicValidationDecoration(), new StyleClassValidationDecoration("validation-error", null)));

        this.propertySheet.getItems().setAll(items);
    }

    public void validate(ActionEvent actionEvent) {
        if (!Objects.equals(this.estate.getLoyalty(), this.loyaltyField.getDoubleValue())) {
            this.estate.setLoyalty(this.loyaltyField.getDoubleValue());
        }

        if (!Objects.equals(this.estate.getTerritory(), this.territoryField.getDoubleValue())) {
            this.estate.setTerritory(this.territoryField.getDoubleValue());
        }

        if (this.estate.getGrantedPrivileges().size() != this.privileges.size() || this.privileges.stream().anyMatch(Privilege::isChanged)) {
            this.estate.getGrantedPrivileges().forEach(i -> this.privileges.stream()
                                                                           .filter(p -> i.getPrivilege().equals(p.getPrivilege()))
                                                                           .findFirst()
                                                                           .ifPresentOrElse(p -> {
                                                                                                if (!Objects.equals(p.getStartDate(), i.getDate())) {
                                                                                                    i.setDate(p.getStartDate());
                                                                                                }

                                                                                                this.privileges.remove(p);
                                                                                            },
                                                                                            () -> this.estate.removeGrantedPrivilege(i.getPrivilege())));
            this.privileges.forEach(p -> this.estate.addGrantedPrivilege(p.getPrivilege(), p.getStartDate()));
        }
    }

    public SaveEstate getEstate() {
        return estate;
    }

    public DoubleProperty territoryValue() {
        return this.territoryField.getObservableDoubleValue();
    }

    public ValidationSupport getValidationSupport() {
        return validationSupport;
    }

    public CustomPropertySheet getPropertySheet() {
        return propertySheet;
    }

    public List<DoubleProperty> getCountryEstatesTerritory() {
        return countryEstatesTerritory;
    }
}
