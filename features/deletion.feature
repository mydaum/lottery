Feature:
    Lotteries can be deleted

    Background:
        Given that I have started lottery

    Scenario: A lottery is deleted by using its name and token
        Given that a lottery has been created
        When I delete that lottery
        Then I get a 204 code

    Scenario: Deleting a non-existing lottery returns a failure code
        When I delete a non-existing lottery
        Then I get a 400 code

    Scenario: Deleting an existing lottery using a bad token returns a failure
        code
        When I delete a that lottery using a bad token
        Then I get a 403 code

    Scenario: Deleting a non-existing lottery using a bad token returns a
        failure code
        When I delete a non-existing lottery using a bad token
        Then I get a 403 code
