############################### Reserve Variables` #########################################
"""
This function add the variables for reserves to the model
"""
function AddVariableSpec(
    ::Type{ActiveServiceVariable},
    ::PSIContainer,
    service::T,
) where {T <: PSY.ReserveNonSpinning}
    return AddVariableSpec(;
        variable_name = make_variable_name(PSY.get_name(service), T),
        binary = false,
        lb_value_func = x -> 0,
        devices_filter_func = x -> PSY.get_available(x),
    )
end

################################## Reserve Requirement Constraint ##########################
function service_requirement_constraint!(
    psi_container::PSIContainer,
    service::SR,
    ::ServiceModel{SR, T},
) where {SR <: PSY.ReserveNonSpinning, T <: AbstractReservesFormulation}
    parameters = model_has_parameters(psi_container)
    use_forecast_data = model_uses_forecasts(psi_container)
    initial_time = model_initial_time(psi_container)
    @debug initial_time
    time_steps = model_time_steps(psi_container)
    name = PSY.get_name(service)
    constraint = get_constraint(psi_container, make_constraint_name(REQUIREMENT, SR))
    reserve_variable = get_variable(psi_container, name, SR)
    use_slacks = get_services_slack_variables(psi_container.settings)

    ts_vector = get_time_series(psi_container, service, "requirement")

    use_slacks && (slack_vars = reserve_slacks(psi_container, name))

    requirement = PSY.get_requirement(service)
    if parameters
        param = get_parameter_array(
            psi_container,
            UpdateRef{SR}(SERVICE_REQUIREMENT, "requirement"),
        )
        for t in time_steps
            param[name, t] = PJ.add_parameter(psi_container.JuMPmodel, ts_vector[t])
            if use_slacks
                resource_expression = sum(reserve_variable[:, t]) + slack_vars[t]
            else
                resource_expression = sum(reserve_variable[:, t])
            end
            constraint[name, t] = JuMP.@constraint(
                psi_container.JuMPmodel,
                resource_expression >= param[name, t] * requirement
            )
        end
    else
        for t in time_steps
            constraint[name, t] = JuMP.@constraint(
                psi_container.JuMPmodel,
                sum(reserve_variable[:, t]) >= ts_vector[t] * requirement
            )
        end
    end
    return
end

function service_requirement_constraint!(
    psi_container::PSIContainer,
    service::SR,
    ::ServiceModel{SR, T},
) where {SR <: PSY.StaticReserveNonSpinning, T <: AbstractReservesFormulation}
    parameters = model_has_parameters(psi_container)
    initial_time = model_initial_time(psi_container)
    @debug initial_time
    time_steps = model_time_steps(psi_container)
    name = PSY.get_name(service)
    constraint = get_constraint(psi_container, make_constraint_name(REQUIREMENT, SR))
    reserve_variable = get_variable(psi_container, name, SR)
    use_slacks = get_services_slack_variables(psi_container.settings)

    use_slacks && (slack_vars = reserve_slacks(psi_container, name))

    requirement = PSY.get_requirement(service)
    for t in time_steps
        resource_expression = JuMP.GenericAffExpr{Float64, JuMP.VariableRef}()
        JuMP.add_to_expression!(resource_expression, sum(reserve_variable[:, t]))
        if use_slacks
            resource_expression += slack_vars[t]
        end
        constraint[name, t] =
            JuMP.@constraint(psi_container.JuMPmodel, resource_expression >= requirement)
    end

    return
end

function cost_function!(
    psi_container::PSIContainer,
    service::SR,
    ::ServiceModel{SR, T},
) where {SR <: PSY.ReserveNonSpinning, T <: AbstractReservesFormulation}
    reserve = get_variable(psi_container, PSY.get_name(service), SR)
    for r in reserve
        JuMP.add_to_expression!(psi_container.cost_function, r, DEFAULT_RESERVE_COST)
    end
    return
end

function get_nonspinning_contributing_devices(
    services_mapping::Dict{ServiceContributingDevicesKey, ServiceContributingDevices}
)
    all_contributing_devices = Vector{PSY.Device}()
    for service in services
        contributing_devices =
            services_mapping[(type = SR, name = PSY.get_name(service))].contributing_devices
        if !isempty(incompatible_device_types)
            contributing_devices =
                [d for d in contributing_devices if typeof(d) ∉ incompatible_device_types]
        end
        push!(all_contributing_devices, contributing_devices...)
    end
    return unique!(all_contributing_devices)
end

function range_limit!(
    psi_container::PSIContainer,
    service::SR,
    contributing_devices::Vector{<:PSY.Device},
) where {SR <: PSY.ReserveNonSpinning}
    reserve_variable = get_variable(psi_container, PSY.get_name(service), SR)
    constraint = get_constraint(psi_container, make_constraint_name(NON_SPINNING_LIMIT, SR))
    for device in contributing_devices
        name = PSY.get_name(device)
        power_limits = PSY.get_active_power_limits(device)
        constraint[name, t] = 
            JuMP.@constraint(psi_container.JuMPmodel, reserve_variable[name, t] <= (power_limits.max - power_limits.min))
    end
    return
end
