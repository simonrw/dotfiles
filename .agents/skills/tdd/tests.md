# Good and Bad Tests

## Good Tests

**Integration-style**: Test through real interfaces, not mocks of internal parts.

```
// GOOD: Tests observable behavior
test "user can checkout with valid cart":
    cart = create_cart()
    cart.add(product)
    result = checkout(cart, payment_method)
    assert result.status == "confirmed"
```

Characteristics:

- Tests behavior users/callers care about
- Uses public API only
- Survives internal refactors
- Describes WHAT, not HOW
- One logical assertion per test

## Bad Tests

**Implementation-detail tests**: Coupled to internal structure.

```
// BAD: Tests implementation details
test "checkout calls payment_service.process":
    mock_payment = mock(payment_service)
    checkout(cart, payment)
    assert mock_payment.process was called with cart.total
```

Red flags:

- Mocking internal collaborators
- Testing private methods
- Asserting on call counts/order
- Test breaks when refactoring without behavior change
- Test name describes HOW not WHAT
- Verifying through external means instead of interface

```
// BAD: Bypasses interface to verify
test "create_user saves to database":
    create_user(name: "Alice")
    row = db.query("SELECT * FROM users WHERE name = ?", ["Alice"])
    assert row is not null

// GOOD: Verifies through interface
test "create_user makes user retrievable":
    user = create_user(name: "Alice")
    retrieved = get_user(user.id)
    assert retrieved.name == "Alice"
```
