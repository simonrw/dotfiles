# When to Mock

Mock at **system boundaries** only:

- External APIs (payment, email, etc.)
- Databases (sometimes - prefer test DB)
- Time/randomness
- File system (sometimes)

Don't mock:

- Your own classes/modules
- Internal collaborators
- Anything you control

## Designing for Mockability

At system boundaries, design interfaces that are easy to mock:

**1. Use dependency injection**

Pass external dependencies in rather than creating them internally:

```
// Easy to mock
function process_payment(order, payment_client):
    return payment_client.charge(order.total)

// Hard to mock
function process_payment(order):
    client = create_stripe_client(env.STRIPE_KEY)
    return client.charge(order.total)
```

**2. Prefer SDK-style interfaces over generic fetchers**

Create specific functions for each external operation instead of one generic function with conditional logic:

```
// GOOD: Each function is independently mockable
api = {
    get_user(id),
    get_orders(user_id),
    create_order(data),
}

// BAD: Mocking requires conditional logic inside the mock
api = {
    fetch(endpoint, options),
}
```

The SDK approach means:
- Each mock returns one specific shape
- No conditional logic in test setup
- Easier to see which endpoints a test exercises
- Type safety per endpoint (in typed languages)
