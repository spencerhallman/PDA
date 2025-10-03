# Product Demonstration Application (PDA) - Business Rules in EARS Format

*Generated: October 3, 2025*  
*Source: COBOL Source Code Analysis*  
*Format: EARS (Easy Approach to Requirements Syntax)*

---

## Table of Contents

1. [PDA001 - Main Menu Program](#pda001---main-menu-program)
2. [PDA002 - Order Menu Program](#pda002---order-menu-program)  
3. [PDA003 - Maintenance Menu Program](#pda003---maintenance-menu-program)
4. [PDA004 - Customer Identification Program](#pda004---customer-identification-program)
5. [PDA005 - Browse Categories Program](#pda005---browse-categories-program)
6. [PDA006 - Browse Items by Category Program](#pda006---browse-items-by-category-program)
7. [PDA007 - Item Detail Program](#pda007---item-detail-program)
8. [PDA010 - Order Inquiry/Maintenance Program](#pda010---order-inquirymaintenance-program)
9. [PDA013 - Base Data Refresh Program](#pda013---base-data-refresh-program)
10. [PDA016 - Customer Order Inquiry (MQSeries)](#pda016---customer-order-inquiry-mqseries)
11. [General Application Rules](#general-application-rules)

---

## PDA001 - Main Menu Program

### User Authentication Rules

**Rule 1.1 - User Login Validation**
WHEN a user attempts to access the PDA application, THE system SHALL verify the user's CICS sign-on ID against the USERID table.

**Rule 1.2 - New User Registration**  
IF a user's CICS sign-on ID does not exist in the USERID table, THEN THE system SHALL automatically create a new user record with a unique user number between 1 and 99,998.

**Rule 1.3 - Maximum User Limit**
IF the system has reached the maximum of 99,998 users, THEN THE system SHALL display error message "UNABLE TO ADD NEW USER, PDA SYSTEM AT MAXIMUM, CONTACT SUPPORT" and deny access.

**Rule 1.4 - User Activity Tracking**
WHEN a user successfully authenticates, THE system SHALL update the LAST_ACCESSED field in the USERID table with the current date.

**Rule 1.5 - User Data Loading**
WHEN a new user is created, THE system SHALL automatically link to program PDA013 to load base demonstration data for the new user.

### Menu Navigation Rules

**Rule 1.6 - Valid Menu Selections**
THE system SHALL only accept menu selections '1' (Orders), '2' (Maintenance), or '3' (Customer Inquiry) as valid options.

**Rule 1.7 - Invalid Menu Selection**
IF the user enters an invalid menu selection, THEN THE system SHALL display error message "THE MENU SELECTION ENTERED IS INVALID".

**Rule 1.8 - Menu Navigation Flow**
WHEN the user selects option '1', THE system SHALL transfer control to program PDA002 (Order Menu).
WHEN the user selects option '2', THE system SHALL transfer control to program PDA003 (Maintenance Menu).  
WHEN the user selects option '3', THE system SHALL transfer control to program PDA016 (Customer Order Inquiry).

### Main Menu Function Key Rules

**Rule 1.9 - Valid Function Keys**
THE system SHALL only accept the following function keys: ENTER, CLEAR, and PF3.

**Rule 1.10 - Invalid Function Key**
IF the user presses an invalid function key, THEN THE system SHALL display error message "INVALID FUNCTION KEY SELECTION - PLEASE RE-SUBMIT".

**Rule 1.11 - Application Exit**
WHEN the user presses PF3 or CLEAR, THE system SHALL display message "PRODUCT DEMONSTRATION APPLICATION SESSION COMPLETE" and terminate the session.

**Rule 1.12 - Mutually Exclusive Operations**
IF the user enters both a menu selection AND presses a function key (other than ENTER), THEN THE system SHALL display error message "ACTION/SELECTION CODE ENTRY WITH PFKEY(S) NOT ALLOWED".

---

## PDA002 - Order Menu Program

### Order Menu Selection Rules

**Rule 2.1 - Valid Order Menu Options**
THE system SHALL only accept the following order menu selections: '1' (Add Order), '2' (Order Inquiry), '3' (Browse Orders), or '4' (Pending Order).

**Rule 2.2 - Order Menu Navigation**
WHEN the user selects option '1', THE system SHALL transfer control to program PDA004 (Customer Identification) for order addition.
WHEN the user selects option '2', THE system SHALL transfer control to program PDA010 (Order Inquiry/Maintenance).
WHEN the user selects option '3', THE system SHALL transfer control to program PDA012 (Browse Submitted Orders).
WHEN the user selects option '4', THE system SHALL transfer control to program PDA008 (Pending Order Processing).

### Order Menu Function Key Rules

**Rule 2.3 - Order Menu Function Keys**
THE system SHALL accept PF12 to return to the main menu (PDA001).

---

## PDA003 - Maintenance Menu Program

### Maintenance Menu Selection Rules

**Rule 3.1 - Valid Maintenance Options**
THE system SHALL only accept the following maintenance selections: '7' (Base Data Refresh), '8' (User ID Utility), or '9' (Product Scenario Selection).

**Rule 3.2 - Maintenance Navigation**
WHEN the user selects option '7', THE system SHALL initiate the base data refresh process.
WHEN the user selects option '8', THE system SHALL display user identification utility functions.
WHEN the user selects option '9', THE system SHALL transfer control to program PDA024 (Product Scenario Selection).

**Rule 3.3 - Data Refresh Confirmation**
WHEN the user selects option '7' for the first time, THE system SHALL display confirmation message "RE-ENTER OPTION 7 TO CONFIRM REFRESH REQUEST".

**Rule 3.4 - Data Refresh Execution**
WHEN the user re-enters option '7' after confirmation, THE system SHALL execute the data refresh process and display "ALL DATA HAS BEEN REFRESHED".

---

## PDA004 - Customer Identification Program

### Customer Validation Rules

**Rule 4.1 - Customer ID Required**
THE system SHALL require a valid customer ID to be entered before proceeding with order processing.

**Rule 4.2 - Customer ID Validation**
WHEN a customer ID is entered, THE system SHALL validate the customer exists in the CUSTOMER VSAM file.

**Rule 4.3 - Invalid Customer**
IF the entered customer ID is not found, THEN THE system SHALL display error message "CUSTOMER ID ENTERED NOT FOUND".

**Rule 4.4 - Customer ID Entry Prompt**
IF no customer ID is entered, THEN THE system SHALL display message "PLEASE ENTER A VALID CUSTOMER ID".

**Rule 4.5 - Customer Information Display**
WHEN a valid customer ID is entered, THE system SHALL display the customer information from the CUSTOMER VSAM file.

### Customer Navigation Rules

**Rule 4.6 - Order Processing Flow**
AFTER successful customer identification for order addition, THE system SHALL transfer control to program PDA005 (Browse Categories).

**Rule 4.7 - Pending Order Flow**  
AFTER successful customer identification for pending order processing, THE system SHALL transfer control to program PDA008 (Pending Order).

**Rule 4.8 - Function Key Navigation**
WHEN PF3 is pressed, THE system SHALL return to program PDA002 (Order Menu).
WHEN PF12 is pressed, THE system SHALL return to program PDA001 (Main Menu).

---

## PDA005 - Browse Categories Program

### Category Selection Rules

**Rule 5.1 - Category Selection Validation**
THE system SHALL only accept 'S' as a valid selection code for category/sub-category selection.

**Rule 5.2 - Selection Code Entry**
IF no selection code is entered, THEN THE system SHALL display message "PLEASE ENTER A SELECTION CODE - 'S'".

**Rule 5.3 - Multiple Selections**
IF more than one selection code is entered, THEN THE system SHALL display message "ONLY ONE SELECTION CODE - 'S' - IS ALLOWED".

**Rule 5.4 - Invalid Selection Code**
IF an invalid selection code is entered, THEN THE system SHALL display message "INVALID SELECTION CODE USED, ENTER 'S'".

### Scrolling Rules

**Rule 5.5 - Forward Scrolling**
WHEN PF8 is pressed and not at the bottom of data, THE system SHALL scroll forward to display the next 15 category/sub-category combinations.

**Rule 5.6 - Backward Scrolling**
WHEN PF7 is pressed and not at the top of data, THE system SHALL scroll backward to display the previous 15 category/sub-category combinations.

**Rule 5.7 - Top of Data**
WHEN attempting to scroll backward at the top of data, THE system SHALL display message "TOP OF DATA REACHED".

**Rule 5.8 - Bottom of Data**
WHEN attempting to scroll forward at the bottom of data, THE system SHALL display message "BOTTOM OF DATA REACHED".

### Category Navigation Rules

**Rule 5.9 - Category Selection Flow**
WHEN a valid category/sub-category selection is made, THE system SHALL transfer control to program PDA006 (Browse Items by Category) with the selected category information.

**Rule 5.10 - Return Navigation**
WHEN PF3 is pressed, THE system SHALL return to program PDA004 (Customer Identification).
WHEN PF12 is pressed, THE system SHALL return to program PDA001 (Main Menu).

---

## PDA006 - Browse Items by Category Program

### Item Selection Rules

**Rule 6.1 - Item Selection Validation**
THE system SHALL only accept 'S' as a valid selection code for item selection.

**Rule 6.2 - Item Selection Processing**
WHEN a valid item selection is made, THE system SHALL transfer control to program PDA007 (Item Detail) for quantity entry and order addition.

### Database Access Rules

**Rule 6.3 - Item Query by Category**
THE system SHALL query the ITEM table in DB2 for all items matching the selected category and sub-category.

**Rule 6.4 - Item Display Limit**
THE system SHALL display a maximum of 12 items per screen page.

### Scrolling and Navigation Rules

**Rule 6.5 - Item Scrolling**
THE system SHALL support forward (PF8) and backward (PF7) scrolling through item lists with appropriate boundary checking.

**Rule 6.6 - Return to Categories**
WHEN PF3 is pressed, THE system SHALL return to program PDA005 (Browse Categories).

---

## PDA007 - Item Detail Program

### Quantity Validation Rules

**Rule 7.1 - Quantity Entry Required**
THE system SHALL require a numeric quantity to be entered for each selected item.

**Rule 7.2 - Quantity Format Validation**
THE system SHALL validate that the entered quantity is numeric and greater than zero.

**Rule 7.3 - Invalid Quantity**
IF an invalid quantity is entered, THEN THE system SHALL display error message "QUANTITY ENTERED IS NOT A VALID VALUE".

**Rule 7.4 - Quantity Entry Prompt**
IF no quantity is entered, THEN THE system SHALL display message "PLEASE ENTER A QUANTITY FOR AN ITEM OR USE A PFKEY".

### Order Addition Rules

**Rule 7.5 - Item Addition to Order**
WHEN valid quantities are entered, THE system SHALL add the selected items to the current order and display message "SELECTED ITEMS HAVE BEEN ADDED TO ORDER".

**Rule 7.6 - Quantity vs Function Key Conflict**
IF the user enters both a quantity AND presses a function key, THEN THE system SHALL display error message "QUANTITY ENTRY WITH PFKEY(S) NOT ALLOWED".

### Purchase Type Rules

**Rule 7.7 - Purchase Type Required**
THE system SHALL require a valid purchase type to be entered for order completion.

**Rule 7.8 - Purchase Type Validation**
THE system SHALL validate the purchase type against the PURCHASE_TYPE table in DB2.

**Rule 7.9 - Invalid Purchase Type**
IF an invalid purchase type is entered, THEN THE system SHALL display error message "INVALID PURCHASE TYPE".

---

## PDA010 - Order Inquiry/Maintenance Program

### Order Number Validation Rules

**Rule 10.1 - Order Number Required**
THE system SHALL require an order number for inquiry or maintenance operations.

**Rule 10.2 - Order Number Format**
THE system SHALL validate that the order number is numeric and non-zero.

**Rule 10.3 - Invalid Order Number**
IF an invalid order number is entered, THEN THE system SHALL display error message "ORDER NUMBER MUST BE NUMERIC AND A NON-ZERO VALUE".

**Rule 10.4 - Order Not Found**
IF the entered order number is not found, THEN THE system SHALL display error message "ORDER NOT FOUND".

### Inquiry Requirements

**Rule 10.5 - Inquiry Before Maintenance**
THE system SHALL require an inquiry operation to be performed before allowing any maintenance operations on an order.

**Rule 10.6 - Inquiry Required Message**
IF maintenance is attempted without prior inquiry, THEN THE system SHALL display error message "AN INQUIRY IS REQUIRED BEFORE THE REQUEST CAN BE PROCESSED".

### Action Code Rules

**Rule 10.7 - Valid Action Codes**
THE system SHALL only accept valid action codes for order maintenance operations.

**Rule 10.8 - Invalid Action Code**
IF an invalid action code is entered, THEN THE system SHALL display error message "INVALID ACTION CODE ENTERED".

**Rule 10.9 - Order Processing Actions**
WHEN PF4 is pressed, THE system SHALL process the order and display message "ORDER HAS BEEN PROCESSED".

**Rule 10.10 - Order Cancellation Actions**
WHEN PF5 is pressed, THE system SHALL cancel the order and display message "ORDER HAS BEEN CANCELLED".

**Rule 10.11 - Confirmation Requirements**
WHEN PF4 is pressed for the first time, THE system SHALL display "PRESS PF4 AGAIN TO CONFIRM ORDER PROCESS".
WHEN PF5 is pressed for the first time, THE system SHALL display "PRESS PF5 AGAIN TO CONFIRM ORDER CANCELLATION".

---

## PDA013 - Base Data Refresh Program

### Data Refresh Rules

**Rule 13.1 - User-Specific Data Loading**
THE system SHALL load demonstration data specific to the user's unique identifier when called for new user setup.

**Rule 13.2 - Data Refresh Processing**
WHEN invoked from the maintenance menu, THE system SHALL refresh all demonstration data for the current user.

**Rule 13.3 - Data Loading Message**
WHILE data loading is in progress, THE system SHALL display message "PLEASE WAIT ....... LOADING USER DATA".

---

## PDA016 - Customer Order Inquiry (MQSeries)

### MQSeries Integration Rules

**Rule 16.1 - Customer Order Query**
THE system SHALL provide customer order inquiry functionality through MQSeries message queuing.

**Rule 16.2 - Query Response Handling**
WHEN a customer order query is submitted, THE system SHALL wait for and process the MQSeries response.

**Rule 16.3 - No Response Handling**
IF no response is received from the MQSeries query, THEN THE system SHALL display error message "INQUIRY FAILED, NO RESPONSE FROM QUERY, PLEASE RE-SUBMIT OR CONTACT SYSTEMS".

**Rule 16.4 - Invalid Transaction Request**
IF an invalid transaction request is made, THEN THE system SHALL display error message "INVALID TRANSACTION REQUEST FOR CUSTOMER ORDER QUERY".

### Customer Identification Rules

**Rule 16.5 - Customer Order Flow**
AFTER customer identification, THE system SHALL display message "ENTER NEW CUSTOMER ID, OR ENTER TO PROCEED WITH INQUIRY".

---

## General Application Rules

### Date Validation Rules

**Rule G.1 - Date Month Validation**
THE system SHALL validate that month values are between 01 and 12 inclusive.

**Rule G.2 - Invalid Month**
IF an invalid month is entered, THEN THE system SHALL display error message "INVALID DATE - MONTH VALUE, MUST BE 01 - 12".

**Rule G.3 - Date Day Validation**
THE system SHALL validate that day values are appropriate for the specified month and year.

**Rule G.4 - Invalid Day**
IF an invalid day is entered, THEN THE system SHALL display error message "INVALID DATE - DAY VALUE".

**Rule G.5 - Date Year Validation**
THE system SHALL validate that year values are within acceptable ranges.

**Rule G.6 - Invalid Year**
IF an invalid year is entered, THEN THE system SHALL display error message "INVALID DATE - YEAR VALUE".

**Rule G.7 - Days Exceed Month Maximum**
IF the day value exceeds the maximum days for the specified month, THEN THE system SHALL display error message "DATE DAY VALUE EXCEEDS THE NUMBER OF DAYS IN THE MONTH".

### Purchase Order Number Rules

**Rule G.8 - Purchase Order Format**
THE system SHALL validate that purchase order numbers are numeric and non-zero values.

**Rule G.9 - Invalid Purchase Order**
IF an invalid purchase order number is entered, THEN THE system SHALL display error message "PURCHASE ORDER NUMBER MUST BE NUMERIC AND A NON-ZERO VALUE".

### Error Handling Rules

**Rule G.10 - General Error Processing**
WHEN any error occurs during processing, THE system SHALL display appropriate error messages and maintain transaction integrity.

**Rule G.11 - Transaction Rollback**
IF a fatal error occurs, THE system SHALL perform automatic transaction rollback and display error information.

**Rule G.12 - Error Recovery**
AFTER displaying an error message, THE system SHALL allow the user to correct the input and retry the operation.

### Security and Access Control Rules

**Rule G.13 - User Session Management**
THE system SHALL maintain user session information throughout the application navigation using the COMMAREA structure.

**Rule G.14 - Transaction Security**
THE system SHALL ensure all database updates are performed within proper CICS transaction boundaries.

**Rule G.15 - User Authorization**
THE system SHALL verify user authorization for all maintenance and update operations.

### System Limits and Constraints

**Rule G.16 - Maximum Users**
THE system SHALL support a maximum of 99,998 concurrent user accounts.

**Rule G.17 - Communication Area Size**
THE system SHALL maintain a fixed communication area size of 2000 bytes for transaction passing.

**Rule G.18 - Screen Display Limits**
THE system SHALL display a maximum of 15 items per screen for browse operations.

**Rule G.19 - Order Item Limits**
THE system SHALL support unlimited order items within system memory constraints.

**Rule G.20 - Message Length**
THE system SHALL limit error and informational messages to 79 characters maximum length.

### Data Integrity Rules

**Rule G.21 - Database Consistency**
THE system SHALL maintain referential integrity across all database operations involving USERID, CUSTOMER, ORDER, and ITEM tables.

**Rule G.22 - Transaction Atomicity**
THE system SHALL ensure all multi-step operations (such as order creation) are atomic and can be completely rolled back if any step fails.

**Rule G.23 - Concurrent Access**
THE system SHALL handle concurrent user access to shared resources through appropriate locking mechanisms.

**Rule G.24 - Data Validation**
THE system SHALL perform comprehensive data validation before committing any changes to persistent storage.

---

## Summary

This document contains **78 business rules** extracted from the Product Demonstration Application source code, organized by program module and formatted using the EARS (Easy Approach to Requirements Syntax) methodology. These rules represent the core business logic embedded in the COBOL application and provide the foundation for understanding system behavior during modernization efforts.

**Rule Distribution by Program:**

- PDA001 (Main Menu): 12 rules
- PDA002 (Order Menu): 3 rules  
- PDA003 (Maintenance Menu): 4 rules
- PDA004 (Customer Identification): 8 rules
- PDA005 (Browse Categories): 10 rules
- PDA006 (Browse Items): 6 rules
- PDA007 (Item Detail): 9 rules
- PDA010 (Order Inquiry/Maintenance): 11 rules
- PDA013 (Data Refresh): 3 rules
- PDA016 (Customer Order Inquiry): 5 rules
- General Application Rules: 24 rules

These rules should be preserved and implemented in any modernized version of the application to maintain business functionality and user experience consistency.
