package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.country.SaveEstate;
import com.osallek.eu4saveeditor.controller.control.TableView2Privilege;
import com.osallek.eu4saveeditor.controller.object.Privilege;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import com.osallek.eu4saveeditor.controller.pane.TableViewDialog;
import com.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ButtonItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderItem;
import com.osallek.eu4saveeditor.controller.validator.CustomGraphicValidationDecoration;
import javafx.beans.binding.DoubleExpression;
import javafx.beans.property.DoubleProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.layout.VBox;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

public class EstatePropertySheet extends VBox {

    private final Country country;

    private final SaveEstate estate;

    private final CustomPropertySheet propertySheet;

    private final ClearableSliderItem loyaltyField;

    private final ClearableSliderItem territoryField;

    private final ButtonItem privilegeButton;

    private final ObservableList<Privilege> privileges;

    private final ValidationSupport validationSupport;

    private CustomPropertySheetSkin propertySheetSkin;

    private final List<DoubleProperty> countryEstatesTerritory = FXCollections.observableArrayList();

    public EstatePropertySheet(Country country, SaveEstate estate) {
        this.country = country;
        this.estate = estate;
        this.propertySheet = new CustomPropertySheet();
        this.propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.propertySheet.setMode(CustomPropertySheet.Mode.CATEGORY);
        this.propertySheet.setModeSwitcherVisible(false);
        this.propertySheet.setSearchBoxVisible(false);

        List<CustomPropertySheet.Item> items = new ArrayList<>();

        this.propertySheetSkin = new CustomPropertySheetSkin(this.propertySheet);
        this.propertySheet.setSkin(this.propertySheetSkin);

        this.loyaltyField = new ClearableSliderItem(this.estate.getEstateGame().getLocalizedName(),
                                                    this.country.getSave().getGame().getLocalisation("LOYALTY"),
                                                    0, 100, this.estate.getLoyalty(), this.estate::getLoyalty);
        items.add(this.loyaltyField);

        this.territoryField = new ClearableSliderItem(this.estate.getEstateGame().getLocalizedName(),
                                                      this.country.getSave().getGame().getLocalisation("TERRITORY"),
                                                      0, 100, this.estate.getTerritory(), this.estate::getTerritory);
        this.territoryField.getObservableDoubleValue().addListener((observable, oldValue, newValue) -> {
            if ((oldValue.doubleValue() == 100d || newValue.doubleValue() == 100d || !Objects.equals(oldValue, newValue))
                && (this.countryEstatesTerritory.stream().mapToDouble(DoubleExpression::doubleValue).sum() + newValue.doubleValue()) > 100d) {
                this.territoryField.getObservableDoubleValue().set(100d - this.countryEstatesTerritory.stream().mapToDouble(DoubleExpression::getValue).sum());
            }
        });
        items.add(this.territoryField);

        this.privileges = FXCollections.observableArrayList(this.estate.getGrantedPrivileges().stream().map(Privilege::new).collect(Collectors.toList()));
        this.privilegeButton = new ButtonItem(this.estate.getEstateGame().getLocalizedName(), null,
                                              country.getSave().getGame().getLocalisationClean("PRIVILEGE_PICKER_TITLE"), 2);
        this.privilegeButton.getButton().setOnAction(event -> {
            TableView2Privilege tableView2Privilege = new TableView2Privilege(this.country, this.estate, this.privileges,
                                                                              FXCollections.observableArrayList(
                                                                                      this.estate.getEstateGame().getPrivileges().values()));
            TableViewDialog<Privilege> dialog =
                    new TableViewDialog<>(this.country.getSave(),
                                          tableView2Privilege,
                                          this.country.getSave().getGame().getLocalisationClean("PRIVILEGE_PICKER_TITLE"),
                                          (list) -> new Privilege(this.estate.getEstateGame()
                                                                             .getPrivileges()
                                                                             .values()
                                                                             .stream()
                                                                             .filter(p -> list.stream()
                                                                                              .noneMatch(p2 -> p2.getPrivilege().equals(p)))
                                                                             .findFirst()
                                                                             .get(),
                                                                  this.country.getSave().getDate()),
                                          () -> this.privileges);
            dialog.setDisableAddProperty(tableView2Privilege.disableAddPropertyProperty());
            Optional<List<Privilege>> privilegeList = dialog.showAndWait();

            privilegeList.ifPresent(this.privileges::setAll);
        });
        items.add(this.privilegeButton);

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
